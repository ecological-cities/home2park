#'Get building polygons from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) building polygons within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'Data is filtered by key-value attributes, where `building:` is not `NULL`.
#'The column `levels` is derived from `building:levels`; values were set to `1` if the
#'extracted value is empty or `NA`, and set to `NA` if `≤ 0` (i.e. underground);
#'values were then rounded up to the nearest whole number.
#'The column `area_m2` represents the building footprint area,
#'and `floorarea_m2` is calculated by multiplying the `area_m2` by the number of `levels`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data.
#'@param driver character (optional). Name of driver used to export output data, passed to `sf::st_write()`.
#'Defaults to "GeoJSON".
#'@param delete_dsn logical (optional). Passed to `sf::st_write()`.
#'@param append defaults to `NA`, which raises an error if a layer exists. Passed to `sf::st_write()`.
#'
#'@return The processed building polygons (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter mutate
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp)
#' pop_sgp <- sf::st_transform(pop_sgp, sf::st_crs(32648)) # transform to projected crs
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
#' # run function
#' get_buildings_osm(place = city_boundaries,
#'                   date = as.Date('2021-01-01'),
#'                   filename = 'buildings_osm-polygons_2021-01-01.geojson')
#' }
#'
#'@export
get_buildings_osm <- function(place, date = NULL, dir_raw = osmextract::oe_download_directory(), filename = NULL, driver = "GeoJSON", delete_dsn = TRUE,
                              append = NA, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_date(date, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = TRUE,
        add = coll)

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # form url link ----
    link <- osmextract::oe_match(place, provider = "geofabrik")$url  # download region

    if (!is.null(date)) {
        date <- format(date, "%y%m%d")
        link <- gsub("latest", date, link)  # change download date if provided
    }


    # parameters to filter data after download ----
    osmkeys <- c("building:levels", "building:use", "building:architecture", "height")  # building & levels already default keys

    q <- "SELECT * FROM 'multipolygons' WHERE building IS NOT NULL"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, sf::st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()



    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = q, wkt_filter = bb)


    # clean up ---- remove empty geoms, transform back to same crs as 'place', cast to
    # individual polygons
    suppressWarnings(results <- results %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid())

    # rm invalid building polygons
    results <- results[sf::st_is_valid(results),]
    results <- results[!sf::st_is_empty(results),]

    results <- results %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::st_cast("POLYGON", warn = FALSE)



    # convert building_levels ---- if no info, set as 1; if underground, NA; convert to
    # numeric, round up
    suppressWarnings(results <- results %>%
        dplyr::mutate(levels = as.numeric(.data$building_levels)) %>%
        dplyr::mutate(levels = ifelse(is.na(.data$levels) | .data$levels == "", 1, ifelse(.data$levels <=
            0, NA, .data$levels))) %>%
        dplyr::mutate(levels = ceiling(.data$levels)))

    # calculate floor area per polygon ----
    results$area_m2 <- sf::st_area(results)
    results <- results %>%
        dplyr::mutate(floorarea_m2 = .data$area_m2 * .data$levels)


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = driver, delete_dsn = delete_dsn, append = append)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}
