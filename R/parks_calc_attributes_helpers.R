#'Calculate the total edge length of a raster associated with each park polygon
#'
#'Helper function within `parks_calc_attributes()`.
#'The total edge length of (classified) raster patches contained [within](https://postgis.net/docs/ST_Within.html)
#'the `polygons` will be calculated. Additionally, the argument `raster_edge_buffer` provides a way include patches
#'in close proximity to the `polygons`. The total edge lengths are summed together and appended to the `polygons`
#'data as additional columns (or one column, if there is only one raster class).
#'Note that this operation may take a while to complete, as it involves the conversion of rasters to polygons (and vice versa).
#'
#'@param polygons `sf` (with projected coordinate reference system).
#'@param raster `SpatRaster` object from `terra::rast()`. Should have a (projected) coordinate reference system similar to `polygons`.
#'@param raster_min_patch_size Minimum patch size to be included in results.
#'Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system. Defaults to `0` m^2.
#'@param raster_edge_buffer numeric. Specify buffer distance to add to polygonised raster; the total edge length of `polygons` that
#'[intersect](https://postgis.net/docs/ST_Intersection.html) the buffered `raster`
#'will be summed up together with the total edge length contained [within](https://postgis.net/docs/ST_Within.html) the `polygons`.
#'Defaults to `0` (only patches fully contained [within](https://postgis.net/docs/ST_Within.html) polygons will be considered).
#'Provided either as a units object (see `units::set_units()`), or a number in the units of the coordinate reference system.
#'@param relative logical. Whether or not to calculate relative amounts
#'(i.e. ratio of edge-to-perimeter length). Defaults to `TRUE`.
#'
#'@return `polygons` with added column(s) `< class value >_length`, and `< class value >_length_perim_ratio` if `relative` is set to `TRUE`.
#'Note that the value `0` will be summarised; convert pixels to `NA` if you wish to exclude them.
#'
#'@import sf
#'@import checkmate
#'@importFrom stars st_as_stars
#'@importFrom lwgeom st_perimeter
#'@importFrom dplyr filter group_by summarise left_join mutate n
#'@importFrom tidyr pivot_wider replace_na
#'@importFrom tidyselect everything
#'@importFrom units set_units
#'@importFrom rlang .data
#'@importFrom methods as
#'
#'@export
raster_edge_length <- function(polygons, raster, raster_min_patch_size = units::set_units(0,
    "m^2"), raster_edge_buffer = units::set_units(0, "m^2"), relative = TRUE) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assertTRUE(!st_is_longlat(polygons) & !is.null(st_crs(polygons)), add = coll)  # must be projected crs
    checkmate::assertTRUE(all(st_is_valid(polygons)), add = coll)

    # crs
    checkmate::assertTRUE(st_crs(raster) == st_crs(polygons))

    checkmate::assert_number(as.numeric(raster_min_patch_size), na.ok = FALSE, lower = 0,
        null.ok = FALSE, add = coll)
    checkmate::assert_number(as.numeric(raster_edge_buffer), na.ok = FALSE, lower = 0, null.ok = FALSE,
        add = coll)

    checkmate::assert_logical(relative, any.missing = FALSE, all.missing = FALSE, len = 1,
        null.ok = FALSE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # make temp id col
    results <- polygons %>%
        dplyr::mutate(id_temporary = row_number())

    # convert to stars object, polygonise, merge polygons with similar pixel values
    raster_sf <- stars::st_as_stars(raster) %>%
        st_as_sf(as_points = FALSE, merge = TRUE, na.rm = TRUE)
    names(raster_sf)[1] <- "raster_classes"  # rename colname for raster classes
    raster_values <- unique(raster_sf$raster_classes)  # get unique classes

    # calc area of each polygon
    raster_sf$area <- st_area(raster_sf)

    # remove polygons with area < raster_min_patch_size
    raster_sf <- raster_sf %>%
        dplyr::filter(.data$area >= raster_min_patch_size)


    # add col stating if raster_sf fully contained within park (just pick the id col)
    rasteredge_sf <- raster_sf %>%
        sf::st_join(results %>%
            dplyr::select(.data$id_temporary), join = st_within, left = TRUE)  # left join

    # rasteredge_sf inside polygons: sum up perimeters per park
    raster_within <- rasteredge_sf %>%
        dplyr::filter(!is.na(.data$id_temporary))
    raster_within$edge_within <- lwgeom::st_perimeter(raster_within)  # add col for perimeter
    suppressMessages(raster_within <- raster_within %>%
        st_set_geometry(NULL) %>%
        dplyr::group_by(.data$id_temporary, .data$raster_classes) %>%
        dplyr::summarise(edge_within = sum(.data$edge_within)))


    # raster_sf partially inside or outside greenspaces: buffer raster_sf & get length of
    # intersecting park border
    raster_outside <- rasteredge_sf %>%
        dplyr::filter(is.na(.data$id_temporary)) %>%
        dplyr::select(-.data$id_temporary)  # remove id col
    rm(rasteredge_sf)

    # buffer
    raster_outside <- sf::st_buffer(raster_outside, raster_edge_buffer) %>%
        st_make_valid()

    # convert polygons to lines
    suppressWarnings(parks_lines <- results %>%
        st_cast("MULTILINESTRING") %>%
        st_cast("LINESTRING"))

    # crop parks_lines based on buffered raster polygons (exc. polygons inside), sum length
    suppressWarnings(parks_lines <- st_intersection(parks_lines, raster_outside))
    rm(raster_outside)
    parks_lines$edge_outside <- st_length(parks_lines)
    suppressMessages(parks_lines <- parks_lines %>%
        st_set_geometry(NULL) %>%
        dplyr::group_by(.data$id_temporary, .data$raster_classes) %>%
        dplyr::summarise(edge_outside = sum(.data$edge_outside)))


    # full_join both edge variables by park id & raster class
    parks_rasteredge <- dplyr::full_join(raster_within, parks_lines, by = c("id_temporary",
        "raster_classes"))
    rm(parks_lines, raster_within)

    # add both together
    parks_rasteredge <- parks_rasteredge %>%
        dplyr::rowwise() %>%
        dplyr::mutate(length = sum(.data$edge_within, .data$edge_outside, na.rm = TRUE)) %>%
        dplyr::select(-.data$edge_within, -.data$edge_outside) %>%
        tidyr::pivot_wider(names_from = .data$raster_classes, values_from = .data$length,
            names_glue = "{raster_classes}_{.value}")


    # left_join to polygons, replace NA w 0
    results <- results %>%
        dplyr::left_join(parks_rasteredge, by = "id_temporary") %>%
        dplyr::mutate(dplyr::across(matches("[0-9]_length"), .fns = ~tidyr::replace_na(.,
            0)))

    rm(parks_rasteredge)


    # calc relative values
    if (relative == TRUE) {

        results <- results %>%
            dplyr::mutate(dplyr::across(paste0(raster_values, "_length"), .fns = function(x) x/.data$perimeter,
                .names = "{.col}_perim_ratio"))
    }

    rm(raster_values)
    return(results)
}

#'Calculate the total class area of a (classified) raster for each park polygon.
#'
#'
#'Helper function within `parks_calc_attributes()`.
#'For each unique pixel value (each representing a specific raster class/category),
#'the total area per class is calculated.
#'
#'@param polygons `sf` (with projected coordinate reference system).
#'@param raster `SpatRaster` object from `terra::rast()`. Should have a (projected) coordinate reference system similar to `polygons`.
#'@param raster_min_patch_size Minimum patch size to be included in results.
#'Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system. Defaults to `0` m^2.
#'@param relative logical. Whether or not to calculate relative amounts
#'(i.e. percentage area). Defaults to `TRUE`.
#'@param ... Other arguments passed to `terra::extract()`.
#'
#'@return `polygons` with added column(s) `< class value >_area`, and `< class value >_area_pct` if `relative` is set to `TRUE`.
#'Note that the value `0` will be summarised; convert pixels to `NA` if you wish to exclude them.
#'
#'@import sf
#'@import checkmate
#'@importFrom terra vect rast rasterize res extract
#'@importFrom stars st_as_stars st_rasterize
#'@importFrom dplyr filter group_by summarise left_join mutate
#'@importFrom tidyr pivot_wider replace_na
#'@importFrom units set_units
#'@importFrom rlang .data
#'
#'@export
raster_class_area <- function(polygons, raster, raster_min_patch_size = units::set_units(0,
    "m^2"), relative = TRUE, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assertTRUE(!st_is_longlat(polygons) & !is.null(st_crs(polygons)), add = coll)  # must be projected crs
    checkmate::assertTRUE(all(st_is_valid(polygons)), add = coll)

    # crs
    checkmate::assertTRUE(st_crs(raster) == st_crs(polygons))

    checkmate::assert_number(as.numeric(raster_min_patch_size), na.ok = FALSE, lower = 0,
        null.ok = FALSE, add = coll)

    checkmate::assert_logical(relative, any.missing = FALSE, all.missing = FALSE, len = 1,
        null.ok = FALSE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # make temp id col
    results <- polygons %>%
        dplyr::mutate(area = st_area(polygons)) %>%
        dplyr::mutate(id_temporary = row_number())


    pixel_res <- terra::res(raster)  # get pixel res for later

    # convert to sf if user specifies min patch size, or if need to calc edge
    if (as.numeric(raster_min_patch_size) > 0) {

        crs_output <- st_crs(raster)  # get crs for later

        raster <- stars::st_as_stars(raster)  # convert to stars object to polygonise (crs may change!)


        # polygonise, merge polygons with similar pixel values
        raster_sf <- st_as_sf(raster, as_points = FALSE, merge = TRUE, na.rm = TRUE)

        # calc area of each polygon st_crs(raster_sf)
        raster_sf$area <- st_area(raster_sf)

        # remove polygons with area < raster_min_patch_size units(raster_sf$area) <- NULL # remove
        # units
        raster_sf <- raster_sf %>%
            dplyr::filter(.data$area >= raster_min_patch_size)


        # convert original raster_sf polygon back to raster
        raster <- stars::st_rasterize(raster_sf, dx = pixel_res[1], dy = pixel_res[2])

        # set crs back to original
        raster <- st_set_crs(raster, crs_output)

        # convert back to terra object (currently no way to convert directly...)
        raster <- as(raster, "Raster")  # convert from stars to raster object
        raster <- terra::rast(raster)

        rm(raster_sf, crs_output)
    }


    # extract values
    suppressWarnings(extracted <- terra::extract(x = raster, y = terra::vect(results), fun = NULL,
        ...))
    rm(raster)
    names(extracted)[-1] <- "raster_classes"  # rename colname for raster classes


    # get area
    suppressMessages(extracted <- extracted %>%
        dplyr::group_by(dplyr::across()) %>%
        dplyr::summarise(area = dplyr::n() * prod(pixel_res)) %>%
        tidyr::drop_na())

    # rename & reshape
    extracted <- extracted %>%
        tidyr::pivot_wider(names_from = .data$raster_classes, values_from = .data$area, names_glue = "{raster_classes}_{.value}") %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), .fns = ~tidyr::replace_na(.,
            0)))


    # append to results
    results <- dplyr::left_join(results, extracted, by = c(id_temporary = "ID")) %>%
        dplyr::mutate(dplyr::across(names(extracted)[-1], .fns = ~tidyr::replace_na(., 0)))

    if (relative == TRUE) {

        # convert to 100 % if exceeds 100 (due to pixel resolution)
        results <- results %>%
            dplyr::mutate(dplyr::across(names(extracted)[-1], .fns = function(x) ifelse((x/units::set_units(.data$area,
                NULL) * 100) > 100, 100, x/units::set_units(.data$area, NULL) * 100), .names = "{.col}_pct"))
    }

    rm(pixel_res, extracted)

    return(results)

}
