#'Calculate attributes per park polygon based on supplied data
#'
#'Summaries will be calculated for each of the supplied datasets (`data_points`, `data_lines`, or `data_rasters`)
#'and appended to the `parks` data as additional columns.
#'Ensure that all have a (projected) coordinate reference system similar to `parks`.
#'
#'
#'@param parks `sf` polygons (with projected coordinate reference system).
#'@param data_points Named list of `sf` object(s) containing data of geometry type `POINT` or `MULTIPOINT`.
#'List names are used to name the park attributes (columns) in the output.
#'@param data_lines Named list of `sf` object(s) containing data of geometry type `LINESTRING` or `MULTILINESTRING`.
#'List names are used to name the park attributes (columns) in the output.
#'@param data_rasters Named list of single `SpatRaster` object(s) from `terra::rast()`.
#'List names are used to name the park attributes (columns) in the output.
#'@param rasters_summarise_fun Function to summarise the raster data passed to `terra::extract()`.
#'Defaults to `NULL`, which tabulates the sum of each unique value in the raster (i.e. classified raster,
#'with numbers each representing a specific class/category) using the function `raster_class_area()`.
#'@param ... Other arguments passed to `terra::extract()`.
#'@param raster_min_patch_size Minimum patch size to be included when tabulating the sum of each raster class.
#'Only relevant if `rasters_summarise_fun = NULL`. Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system. Defaults to `0` m^2.
#'@param raster_edge numeric. Option to calculate total edge length of (classified) raster(s)
#'associated with each of the `parks` polygons, using the function `raster_edge_length()`. The total edge length of raster patches contained [within](https://postgis.net/docs/ST_Within.html)
#'`parks` will be calculated, and patches in close proximity can be included by increasing the value of this argument;
#'the length of `parks` borders that [intersect](https://postgis.net/docs/ST_Intersection.html) the buffered raster will be calculated.
#'This provides a way include patches in close proximity to the `parks` (e.g. total waterfront length close to parks).
#'Note that each class (unique value in the raster) will be summarised, including `0`; convert pixels to `NA` if you wish to exclude them.
#'Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system. Defaults to `NULL` (not calculated). Set to `0` to include patches
#'[within](https://postgis.net/docs/ST_Within.html) `parks` only.
#'@param relative logical. Whether or not to calculate relative amounts
#'(e.g. point density, ratio of line-to-perimeter length, proportional area). Defaults to `TRUE`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'
#'@return `parks` with added columns containing the summaries of basic park attributes
#'(area and perimeter), as well as summaries of each of the supplied datasets.
#'The summary method depends on geometry type (i.e. points, lines or rasters) and supplied arguments.
#'Some examples:
#' \describe{
#'   \item{area}{Area of park polygon.}
#'   \item{perimeter}{Perimeter of park polygon.}
#'   \item{< object name in `data_points` >_count}{Total count of points within park polygon.}
#'   \item{< object name in `data_points` >_ptdensity}{Total count divided by area of the park polygon (point density).
#'   Included if argument `relative` set to `TRUE`.}
#'   \item{< object name in `data_lines` >_length}{Sum of line lengths within park polygon.}
#'   \item{< object name in `data_lines` >_length_perim_ratio}{Ratio of line-to-perimeter length of the park polygon.
#'   Included if argument `relative` set to `TRUE`.}
#'   \item{< object name in `data_rasters` >}{Summarised values of a specific raster class
#'   (depends on function provided in the `rasters_summarise_fun` argument).}
#'   \item{< object name in `data_rasters` >< class value >_area}{Total area of a specific raster class
#'   (if `rasters_summarise_fun = NULL`).}
#'   \item{< object name in `data_rasters` >< class value >_area_pct}{Percentage area of a specific raster class
#'   (if `rasters_summarise_fun = NULL`). Included if argument `relative` set to `TRUE`.}
#'   \item{< object name in `data_rasters` >< class value >_length}{Total edge length of a specific raster class.}
#'   \item{< object name in `data_rasters` >< class value >_length_perim_ratio}{Edge-to-perimeter length of a specific raster class.
#'   Included if argument `relative` set to `TRUE`.}
#'  }
#'
#'@import sf
#'@import checkmate
#'@importFrom lwgeom st_perimeter
#'@importFrom terra vect rast rasterize res extract
#'@importFrom stars st_as_stars st_rasterize
#'@importFrom dplyr filter group_by summarise left_join mutate
#'@importFrom tidyr replace_na drop_na
#'@importFrom units set_units
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' parks_sgp <- data(parks_sgp) # park polygons
#'
#' # get playground points first
#' city_boundaries <- data(singapore) %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, 'km^2'))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, 'km^2'))  %>%
#'    sf::st_make_valid()
#'
#' playgrounds <- get_playgrounds_osm(place = city_boundaries,
#'                                    date = as.Date('2021-01-01'))
#' point_list <- list(playgrounds) # convert to list (can add other point data too)
#' names(point_list) <- c("playground") # name each element in list
#'
#'
#' # point count & density per park
#' parks_calc_attributes(parks = parks_sgp,
#'                       data_points = point_list,
#'                       relative = TRUE)
#' }
#'
#'@export
parks_calc_attributes <- function(parks, data_points = list(), data_lines = list(), data_rasters = list(),
    rasters_summarise_fun = NULL, raster_min_patch_size = units::set_units(0, "m^2"), raster_edge = NULL,
    relative = TRUE, filename = NULL, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assertTRUE(!st_is_longlat(parks) & !is.null(st_crs(parks)), add = coll)  # must be projected crs
    checkmate::assertTRUE(all(st_is_valid(parks)), add = coll)

    checkmate::assert_logical(relative, any.missing = FALSE, all.missing = FALSE, len = 1,
        null.ok = FALSE, add = coll)

    checkmate::assert_list(data_points, any.missing = FALSE, all.missing = TRUE, unique = TRUE,
        null.ok = FALSE, add = coll)
    checkmate::assert_list(data_lines, any.missing = FALSE, all.missing = TRUE, unique = TRUE,
        null.ok = FALSE, add = coll)
    checkmate::assert_list(data_rasters, any.missing = FALSE, all.missing = TRUE, unique = TRUE,
        null.ok = FALSE, add = coll)


    # all crs similar
    if (length(data_points) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(data_points, st_crs), function(x) x ==
            st_crs(parks)))))
        checkmate::assertTRUE(!is.null(names(data_points)), add = coll)
    }
    if (length(data_lines) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(data_lines, st_crs), function(x) x ==
            st_crs(parks)))))
        checkmate::assertTRUE(!is.null(names(data_lines)), add = coll)
    }
    if (length(data_rasters) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(data_rasters, st_crs), function(x) x ==
            st_crs(parks)))))
        checkmate::assertTRUE(!is.null(names(data_rasters)), add = coll)

        checkmate::assert_number(as.numeric(raster_min_patch_size), na.ok = FALSE, lower = 0,
            null.ok = FALSE, add = coll)
        checkmate::assert_number(raster_edge, na.ok = FALSE, lower = 0, null.ok = TRUE, add = coll)

    }

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # basic attributes ----
    results <- parks %>%
        dplyr::mutate(area = st_area(parks)) %>%
        dplyr::mutate(perimeter = lwgeom::st_perimeter(parks)) %>%
        dplyr::mutate(id_temporary = row_number())


    # points ---- only runs if not empty list
    for (i in seq_along(data_points)) {

        attribute <- names(data_points)[i]  # get data name

        # count no. of points, append to results
        results[[paste0(attribute, "_count")]] <- lengths(st_intersects(results, data_points[[i]]))

        if (relative == TRUE) {
            results[[paste0(attribute, "_ptdensity")]] <- results[[paste0(attribute, "_count")]]/results$area
        }

        rm(attribute, i)
    }


    # lines ----
    for (i in seq_along(data_lines)) {

        attribute <- names(data_lines)[i]  # get data name

        suppressWarnings(data_lines[[i]] <- st_intersection(data_lines[[i]], results))  # subset lines
        data_lines[[i]]$length <- st_length(data_lines[[i]])  # new col for line length

        # sum length of lines per park id
        data_lines[[i]] <- data_lines[[i]] %>%
            dplyr::group_by(.data$id_temporary) %>%
            dplyr::summarise("length" = sum(.data$length, na.rm = TRUE)) %>%
            dplyr::rename_with(~paste0(attribute, "_", {.x}), .cols = "length")
        st_geometry(data_lines[[i]]) <- NULL

        # append to results
        results <- results %>%
            dplyr::left_join(data_lines[[i]], by = "id_temporary")
        results[[paste0(attribute, "_length")]][is.na(results[[paste0(attribute, "_length")]])] <- 0  # replace na with 0

        if (relative == TRUE) {
            results[[paste0(attribute, "_length_perim_ratio")]] <- results[[paste0(attribute, "_length")]]/results$perimeter
        }

        rm(attribute, i)
    }


    # rasters ----
    for (i in seq_along(data_rasters)) {

        attribute <- names(data_rasters)[i]  # get data name


        # calc total edge of raster per park polygon
        if (!is.null(raster_edge)) {

            # run function, overwrite results var
            results <- raster_edge_length(polygons = results, raster = data_rasters[[i]],
                raster_min_patch_size = raster_min_patch_size, raster_edge_buffer = raster_edge,
                relative = relative) %>%
                dplyr::rename_with(~paste0(attribute, "_", {
                  .x
                }), .cols = matches("[0-9]_length") | matches("[0-9]_length_perim_ratio"))

        }


        # area of each raster class (if fun = NULL)
        if (is.null(rasters_summarise_fun)) {

            results <- raster_class_area(polygons = results, raster = data_rasters[[i]], raster_min_patch_size = raster_min_patch_size,
                relative = relative, ...) %>%
                dplyr::rename_with(~paste0(attribute, "_", {
                  .x
                }), .cols = matches("[0-9]_area") | matches("[0-9]_area_pct"))


            # if 'fun' argument contained a function (e.g. sum, mean, etc.)
        } else {

            # extract values
            suppressWarnings(extracted <- terra::extract(x = data_rasters[[i]], y = vect(results),
                fun = rasters_summarise_fun, ...))

            extracted <- extracted %>%
                tidyr::drop_na()
            names(extracted)[2] <- attribute  # rename

            # append to results
            results <- dplyr::left_join(results, extracted, by = c(id_temporary = "ID")) %>%
                dplyr::mutate(dplyr::across(attribute, .fns = ~tidyr::replace_na(., 0)))

        }

        rm(attribute, i)
    }


    # remove temp id column ----
    results <- results %>%
        dplyr::select(-.data$id_temporary)


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    return(results)
}

