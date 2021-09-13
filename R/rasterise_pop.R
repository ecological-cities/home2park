#'Rasterise population counts within census block polygons
#'
#'Convert population counts per census block (polygons)
#'into population density grid (raster) for subsequent analyses.
#'Each polygon's count is divided by its area, such that the integrated
#'density over each census block should equal the original count.
#'Census data for multiple years can be processed (one raster per year).
#'The column name for the `year` must be specified, even if data does not contain multiple years
#'(combined data across multiple years must be in the 'long' format).
#'
#'@param sf `sf`polygons containing the population census data.
#'Data should be in a projected coordinate reference system (for calculation of pixel resolution).
#'@param res number. Specify the pixel resolution for rasterisation (in metres). Defaults to `10`.
#'@param census_block character. Specify column name of the census block unique identifier (e.g. name).
#'@param pop_count character. Specify column name of the population counts per census block.
#'@param year character. Specify column name of the census year. Defaults to `'year'`.
#'@param dir_processing character. Directory to store intermediate files. Defaults to `tempdir()`.
#'Set to `NULL` if you do not wish to export intermediate (temporary) files for subsequent processing.
#'
#'@return List containing (1) rasterised population count per census block,
#'(2) rasterised population density per census block, and (3) processed polygons used to generate
#'these rasters. Named `pop_count_rasters`, `pop_density_rasters` and `pop_polygons`, respectively. List is nested if multiple years are present (one sub-list for each census year).
#'Zero values for population counts/density are converted to `NA`. Intermediate (raster) files are exported to `tempdir()` for further processing.
#'
#'@import sf
#'@import checkmate
#'@importFrom glue glue
#'@importFrom dplyr filter mutate
#'@importFrom raster raster writeRaster
#'@importFrom terra vect rast rasterize
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp)
#'
#'
#' # transform to projected crs
#' pop_sgp <- sf::st_transform(pop_sgp, sf::st_crs(32648))
#'
#'
#' # run function
#' pop_rasters <- rasterise_pop(pop_sgp,
#'                              res = 10,
#'                              census_block = "subzone_n",
#'                              pop_count = "pop_count",
#'                              year = "year")
#' }
#'
#'@export
rasterise_pop <- function(sf, res = 10, census_block = NULL, pop_count = NULL, year = "year",
    dir_processing = tempdir()) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # file formats
    checkmate::assertTRUE(!st_is_longlat(sf) & !is.null(st_crs(sf)), add = coll)  # must be projected crs
    checkmate::assertTRUE(all(st_is_valid(sf)), add = coll)  # all features must be valid

    # resolution
    checkmate::assert_number(res, lower = 0, finite = TRUE, null.ok = FALSE, add = coll)

    # colnames
    checkmate::assert_subset(pop_count, choices = colnames(sf), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(census_block, choices = colnames(sf), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(year, choices = colnames(sf), empty.ok = FALSE, add = coll)

    # filepaths
    checkmate::assert_character(dir_processing, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    sf <- sf %>%
        dplyr::mutate(area = st_area(sf)) %>%
        dplyr::mutate(pop_perpixel = units::drop_units(.data[[pop_count]]/.data$area * (res * res)))  # drop units


    # template raster file - reference for downstream calculations (& export)
    raster_template <- sf %>%
        raster::raster(res = res)  # make & export using library('raster'), cuz library('terra') fails to export if no cell values

    suppressWarnings(raster::writeRaster(raster_template, filename = file.path(glue::glue("{dir_processing}/popdensity_raster-template.tif")),
        overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW"))))


    # results objects
    results <- list()  # create empty list for output
    results[[1]] <- list()
    results[[2]] <- list()
    results[[3]] <- list()
    names(results) <- c("pop_count_rasters", "pop_density_rasters", "pop_polygons")


    # If census year was specified
    if (!is.null(year))
        {

            years <- unique(sf[[year]])

            # process for each year
            for (i in seq_along(years)) {

                sf_subset <- sf %>%
                  dplyr::filter(.data[[year]] == years[i])  # %>%
                # dplyr::mutate(ID = as.numeric(as.factor(.data[[census_block]]))) # ID cols as numbers,
                # to include 0 or missing when rasterised

                # rasterize & export to temp directory
                results[[1]][[i]] <- terra::rasterize(terra::vect(sf_subset), terra::rast(raster_template),
                  field = pop_count)  # pop count
                results[[1]][[i]][results[[1]][[i]] == 0] <- NA  # convert 0s to NAs

                results[[2]][[i]] <- terra::rasterize(terra::vect(sf_subset), terra::rast(raster_template),
                  field = "pop_perpixel")  # pop density
                results[[2]][[i]][results[[2]][[i]] == 0] <- NA

                # export
                names(results[[1]][[i]]) <- glue::glue("popcount-by-census-block_{years[i]}")
                # terra::writeRaster(results[[1]][[i]], filename =
                # file.path(glue::glue('{tempdir()}/{names(results[[1]][[i]])}.tif')), overwrite = TRUE,
                # wopt = list(gdal=c('COMPRESS=LZW')))

                names(results[[2]][[i]]) <- glue::glue("popdensity-by-census-block_{years[i]}")
                # export output imported by filename in subsequent processing
                terra::writeRaster(results[[2]][[i]], filename = file.path(glue::glue("{dir_processing}/{names(results[[2]][[i]])}.tif")),
                  overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

                # raw file
                results[[3]][[i]] <- sf_subset
                names(results[[3]])[i] <- glue::glue("popdata_{years[i]}")
                # st_write(results[[3]][[i]], glue::glue('{tempdir()}/{names(results[[3]])[i]}.geojson'),
                # driver = 'GeoJSON', delete_dsn = TRUE, overwrite = TRUE, quiet = TRUE)

                rm(i)
                terra::tmpFiles(remove = TRUE)  # remove temp files to clear memory
            }


            # If census year was not specified
        }  # else{
    # # sf <- sf %>% # mutate(ID = as.factor(.data[[census_block]])) # rasterize & export to
    # temp directory results[[1]] <- terra::rasterize(terra::vect(sf),
    # terra::rast(raster_template), field = pop_count) # pop count results[[1]][results[[1]]
    # == 0] <- NA # convert 0s to NAs results[[2]] <- terra::rasterize(terra::vect(sf),
    # terra::rast(raster_template), field = 'pop_perpixel') results[[2]][results[[2]] == 0] <-
    # NA # convert 0s to NAs # export names(results[[1]]) <- glue::glue('pop-census-blocks')
    # terra::writeRaster(results[[1]], filename =
    # file.path(glue::glue('{tempdir()}/{names(results[[1]])}.tif')), overwrite = TRUE, wopt =
    # list(gdal=c('COMPRESS=LZW'))) names(results[[1]]) <-
    # glue::glue('popcount-by-census-block') terra::writeRaster(results[[1]], filename =
    # file.path(glue::glue('{tempdir()}/{names(results[[1]])}.tif')), overwrite = TRUE, wopt =
    # list(gdal=c('COMPRESS=LZW'))) names(results[[2]]) <- 'popdensity-by-census-block'
    # terra::writeRaster(results[[2]], filename =
    # file.path(glue::glue('{dir_processing}/{names(results[[2]])}.tif')), overwrite = TRUE,
    # wopt = list(gdal=c('COMPRESS=LZW'))) # raw file results[[3]] <- sf names(results[[3]])
    # <- glue::glue('popdata') st_write(sf_subset,
    # glue::glue('{tempdir()}/{names(results[[3]])}.geojson'), driver = 'GeoJSON', delete_dsn
    # = TRUE, overwrite = TRUE, quiet = TRUE) }

    terra::tmpFiles(remove = TRUE)  # remove temp files to clear memory
    return(results)
}
