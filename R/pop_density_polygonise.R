#'Polygonise population density raster to population count per (building) polygon
#'
#'Convert raster of population density (e.g. from `pop_dasymap()` output) to population count per polygon
#'(adjacent polygons with similar pixel values are merged).
#'Thus, if rasters supplied to `pop_dasymap()` are at a spatial resolution small enough to delineate individual buildings,
#'conversion to polygons using this function would reflect the population count per building.
#'Input data should have a projected coordinate reference system specific to the target area.
#'
#'
#'@param input_raster Population density raster as a SpatRaster object (`terra::rast()`).
#'Output from `pop_dasymap()` may be used.
#'@param write Whether or not to export the output. Defaults to `TRUE`.
#'@param dsn Argument passed to `sf::st_write()`.
#'@param driver character. Argument passed to `sf::st_write()`. Defaults to `'GeoJSON'`.
#'@param overwrite logical. Argument passed to `sf::st_write()`. Defaults to `TRUE`.
#'@param delete_dsn Argument passed to `sf::st_write()`. Defaults to `TRUE`.
#'@param ... Other arguments passed to `sf::st_write()`.
#'
#'@return Building `sf `polygons with column `popcount`.
#'
#'@import sf
#'@import checkmate
#'@importFrom dplyr mutate select
#'@importFrom stars st_as_stars
#'@importFrom terra res
#'@importFrom units drop_units
#'@importFrom rlang .data
#'
#'@export
pop_density_polygonise <- function(input_raster, write = TRUE, dsn, driver = "GeoJSON", overwrite = TRUE, 
    delete_dsn = TRUE, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # file formats
    checkmate::assertTRUE(!st_is_longlat(input_raster) & !is.null(st_crs(input_raster)), add = coll)  # must be projected crs

    checkmate::assert_logical(write, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    pixel_res <- terra::res(input_raster)  # get pixel res for later
    crs_output <- st_crs(input_raster)  # get crs for later

    input_raster <- stars::st_as_stars(input_raster)  # convert to stars object to polygonise (crs changes!)

    # polygonise, merge polygons with similar pixel values
    output_polygons <- st_as_sf(input_raster, as_points = FALSE, merge = TRUE) %>%
        st_make_valid()


    # convert polygon pop density values to pop count per polygon (e.g. building)
    colnames(output_polygons)[1] <- "pop_perpixel"
    output_polygons$area_m2 <- st_area(output_polygons)

    output_polygons <- output_polygons %>%
        dplyr::mutate(popcount = .data$pop_perpixel * .data$area_m2/prod(pixel_res)) %>%
        dplyr::select(-.data$area_m2, -.data$pop_perpixel) %>%
        dplyr::mutate(popcount = units::drop_units(.data$popcount))

    # set crs back to original
    output_polygons <- st_set_crs(output_polygons, crs_output)


    # export
    if (write == TRUE) {
        st_write(output_polygons, dsn = dsn, overwrite = overwrite, driver = driver, delete_dsn = delete_dsn, 
            ...)
    }

    rm(pixel_res, crs_output)

    return(output_polygons)
}
