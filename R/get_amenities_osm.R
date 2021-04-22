#'Get public playgrounds from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) public playgrounds (points) within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM points filtered by key-value attributes, where `leisure:fitness_station` or `sport:*`,
#'and `access:` is not `no` or `private`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed playgrounds (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' city_boundaries <- data(singapore) %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, "km^2"))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, "km^2"))  %>%
#'    sf::st_make_valid()
#'
#' get_playgrounds_osm(place = city_boundaries,
#'                     date = as.Date("2021-01-01"),
#'                     filename = "public-playgrounds_osm-points_2021-01-01.geojson")
#' }
#'
#'@export
get_playgrounds_osm <- function(place, date = NULL, dir_raw = oe_download_directory(), filename = NULL,
    ...) {

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
    osmkeys <- c("leisure", "access")

    q <- "SELECT * FROM 'points' WHERE leisure = 'playground' AND (access IS NULL OR access NOT IN ('no', 'private'))"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()


    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "points", extra_tags = osmkeys,
        force_vectortranslate = TRUE, query = q, wkt_filter = bb)


    # clean up ---- remove empty geoms, transform back to same crs as 'place'
    suppressWarnings(results <- results %>%
                         dplyr::filter(!sf::st_is_empty(.)) %>%
                         sf::st_transform(sf::st_crs(place)) %>%
                         sf::st_make_valid())


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}

#'Get public sports/fitness amenities from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) public sports/fitness amenities (points) within `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM points are filtered by key-value attributes, where `leisure:playground`, and `access:` is not `no` or `private`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed sport/fitness amenities (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' city_boundaries <- data(singapore) %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, "km^2"))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, "km^2"))  %>%
#'    sf::st_make_valid()
#'
#' get_sportfitness_osm(place = city_boundaries,
#'                     date = as.Date("2021-01-01"),
#'                     filename = "sport-fitness_osm-points_2021-01-01.geojson")
#' }
#'
#'@export
get_sportfitness_osm <- function(place, date = NULL, dir_raw = oe_download_directory(), filename = NULL,
    ...) {

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
    osmkeys <- c("leisure", "sport", "access")

    q <- "SELECT * FROM 'points' WHERE (leisure = 'fitness_station' OR sport IS NOT NULL) AND (access IS NULL OR access NOT IN ('no', 'private'))"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()


    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "points", extra_tags = osmkeys,
        force_vectortranslate = TRUE, query = q, wkt_filter = bb)


    # clean up ---- remove empty geoms, transform back to same crs as 'place'
    suppressWarnings(results <- results %>%
                         dplyr::filter(!sf::st_is_empty(.)) %>%
                         sf::st_transform(sf::st_crs(place)) %>%
                         sf::st_make_valid())


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}

#'Get accessible trails from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) accessible trail (lines) within `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM lines filtered by key-value attributes, where `highway:` is `track`, `path`, `footway` or `cycleway`, and `access:` is not `no` or `private`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed trail lines (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' city_boundaries <- data(singapore) %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, "km^2"))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, "km^2"))  %>%
#'    sf::st_make_valid()
#'
#' get_trails_osm(place = city_boundaries,
#'                date = as.Date("2021-01-01"),
#'                filename = "accessible-trails_osm-lines_2021-01-01.geojson")
#' }
#'
#'@export
get_trails_osm <- function(place, date = NULL, dir_raw = oe_download_directory(), filename = NULL,
    ...) {

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
    osmkeys <- c("access")  # 'highway' already included in default keys

    q <- "SELECT * FROM 'lines' WHERE (highway IN ('track', 'path', 'footway', 'cycleway')) AND (access IS NULL OR access NOT IN ('no', 'private'))"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()



    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "lines", extra_tags = osmkeys,
        force_vectortranslate = TRUE, query = q, wkt_filter = bb)


    # clean up ---- remove empty geoms, transform back to same crs as 'place'
    suppressWarnings(results <- results %>%
                         dplyr::filter(!sf::st_is_empty(.)) %>%
                         sf::st_transform(sf::st_crs(place)) %>%
                         sf::st_make_valid())


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}
