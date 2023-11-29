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
#'@param write Whether or not to export the output. Defaults to `FALSE`.
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
#'@importFrom terra res
#'@importFrom units drop_units
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp) # population census block polygons
#' data(landuse_sgp) # land use polygons
#'
#'
#' # transform to projected crs
#' pop_sgp <- sf::st_transform(pop_sgp, sf::st_crs(32648))
#' landuse_sgp <- sf::st_transform(landuse_sgp, sf::st_crs(32648))
#'
#'
#' # merge all census blocks for chosen year (2020) into single multi-polygon
#' # function requires that polygons are merged
#' city_boundaries <- pop_sgp %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, 'km^2'))  %>% # clean up
#'    smoothr::drop_crumbs(threshold = units::set_units(1, 'km^2'))  %>%
#'    sf::st_make_valid()
#'
#' buildings <- get_buildings_osm(place = city_boundaries,
#'                                date = as.Date('2021-01-01')) %>%
#'    dplyr::mutate(year = 2020)
#'
#'
#' # rasterise population, landuse & buildings
#' pop_rasters <- rasterise_pop(pop_sgp,
#'                              census_block = "subzone_n",
#'                              pop_count = "pop_count")
#'
#' landuse_rasters <- rasterise_landuse(landuse_sgp,
#'                                      land_use = 'lu_desc',
#'                                      subset = c('1' = 'RESIDENTIAL',
#'                                                 '2' = 'COMMERCIAL & RESIDENTIAL',
#'                                                 '3' = 'RESIDENTIAL WITH COMMERCIAL AT 1ST STOREY',
#'                                                 '4' = 'RESIDENTIAL / INSTITUTION'),
#'                                      sf_pop = pop_sgp,
#'                                      match_landuse_pop = 'recent')
#'
#' buildings_rasters <- rasterise_buildings(buildings,
#'                                          proxy_pop_density = 'levels',
#'                                          year = 'year',
#'                                          sf_pop = pop_sgp,
#'                                          sf_landuse = landuse_sgp,
#'                                          match_buildings_pop = 'closest')
#'
#'
#' # perform dasymetric mapping on selected year (2020)
#' popdens_raster <- pop_dasymap(pop_polygons = pop_rasters$pop_polygons[[2]],
#'                               pop_perblock_count = pop_rasters$pop_count[[2]],
#'                               pop_perblock_density = pop_rasters$pop_density[[2]],
#'                               land_relative_density = buildings_rasters[[2]],
#'                               filename = 'buildings_popdensity.tif',
#'                               wopt = list(gdal=c('COMPRESS=LZW')))
#'
#'
#' # finally, convert to population count per building polygon
#' pop_density_polygonise(input_raster = popdens_raster,
#'                        write = TRUE,
#'                        dsn = 'buildings_popcount.geojson')
#' }
#'
#'@export
pop_density_polygonise <- function(input_raster, write = FALSE, dsn, driver = "GeoJSON", overwrite = TRUE,
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

    output_polygons <-
      terra::as.polygons(input_raster,
                         round = FALSE, # don't round off values before aggregation
                         aggregate = TRUE) %>%  # combine cells with same values
      sf::st_as_sf() %>%
      sf::st_make_valid()

    # convert polygon pop density values to pop count per polygon (e.g. building)
    colnames(output_polygons)[1] <- "pop_perpixel"
    output_polygons$area_m2 <- sf::st_area(output_polygons)

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
