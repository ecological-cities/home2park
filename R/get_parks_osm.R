#'Get public parks from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) park polygons within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM polygons are filtered by key-value attributes, where `leisure:` is `park`, `garden` or `nature_reserve`,
#'and `access:` is not `no` or `private`, as well as the key-value pair `tourism:zoo`.
#'If `mutually_exclusive_with` is provided, [intersections](https://postgis.net/docs/ST_Intersection.html) between
#'the output and these polygon(s) will be excluded using `polygons_mutually_exclude()`.
#'Polygons are then cleaned up using `polygons_clean()`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param mutually_exclusive_with list of `sf` object(s). This may be used to
#'ensure that polygons (e.g. parks, beaches, informal nature areas) are mutually-exclusive (i.e. non-overlapping).
#'Remove output polygons [contained](https://postgis.net/docs/ST_Contains.html) within, as well as [intersections](https://postgis.net/docs/ST_Intersection.html) between,
#'each element of this list. Should have the same coordinate reference system as `place`.
#'@param snap_tolerance numeric. Argument for `tolerance` level passed to `sf::st_snap()`,
#'used to rectify nearly coincident edges between polygons before processing (e.g. `sf::st_contains()`, `sf::st_covers()`).
#'Provided either as a units object (see `units::set_units()`), or a number in the units of the coordinate reference system.
#'Defaults to `5`. Set to `0` if you do not wish to rectify minor overlaps.
#'@param min_area numeric. Specify minimum area of each polygon to be retained in the output,
#'passed to argument `threshold` in `smoothr::drop_crumbs()`.
#'Provided either as a units object (see `units::set_units()`), or a number in the units of
#'the coordinate reference system. Defaults to `0` m^2.
#'@param aggregate_polygons numeric. Argument for `dist` passed to `sf::st_buffer()`.
#'Buffered polygons that overlap will be aggregated into multipolygons.
#'Set to `NULL` if you do not wish to aggregate to multipolygons.
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed park polygons (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter mutate
#'@importFrom units set_units
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp)
#'
#' # merge all census blocks for chosen year (2020) into single multi-polygon
#' # function requires that polygons are merged
#' city_boundaries <- pop_sgp %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, 'km^2'))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, 'km^2'))  %>%
#'    sf::st_make_valid()
#'
#' # run function
#' get_parks_osm(place = city_boundaries,
#'               date = as.Date('2021-01-01'),
#'               snap_tolerance = 5,
#'               aggregate_polygons = 15,
#'               filename = 'public-parks_osm-polygons_2021-01-01.geojson')
#' }
#'
#'@export
get_parks_osm <- function(place, date = NULL, mutually_exclusive_with = list(), snap_tolerance = 5,
    min_area = units::set_units(0, "m^2"), aggregate_polygons = 15, dir_raw = osmextract::oe_download_directory(),
    filename = NULL, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_date(date, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = TRUE,
        add = coll)
    checkmate::assert_list(mutually_exclusive_with, any.missing = FALSE, all.missing = TRUE,
        unique = TRUE, null.ok = FALSE, add = coll)

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    # all crs similar
    if (length(mutually_exclusive_with) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(mutually_exclusive_with, sf::st_crs), function(x) x ==
            sf::st_crs(place)))))
    }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # form url link ----
    link <- osmextract::oe_match(place, provider = "geofabrik")$url  # download region

    if (!is.null(date)) {
        date <- format(date, "%y%m%d")
        link <- gsub("latest", date, link)  # change download date if provided
    }


    # parameters to filter data after download ----
    osmkeys <- c("access")  # leisure & tourism already default keys

    q <- "SELECT * FROM 'multipolygons' WHERE (leisure IN ('park', 'garden', 'nature_reserve') AND (access IS NULL OR access NOT IN ('no', 'private'))) OR tourism = 'zoo'"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, sf::st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()


    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = q, wkt_filter = bb)

    # transform back to same crs as 'place', then remove empty geoms, cast to individual
    # polygons
    suppressWarnings(results <- results %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))


    # run helper functions ----
    if (length(mutually_exclusive_with) != 0) {

        for (i in seq_along(mutually_exclusive_with)) {

            results <- polygons_mutually_exclude(input = results, mutually_exclude = mutually_exclusive_with[[i]],
                snap_tolerance = snap_tolerance)
        }
    }

    results <- polygons_clean(results, snap_tolerance = snap_tolerance, min_area = min_area,
        aggregate_polygons = aggregate_polygons)


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}

#'Get public beaches from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) beach polygons within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM polygons are filtered by key-value attributes, where `natural:beach`, and `access:` is not `no` or `private`.
#'If `mutually_exclusive_with` is provided, [intersections](https://postgis.net/docs/ST_Intersection.html) between
#'the output and these polygon(s) will be excluded using `polygons_mutually_exclude()`.
#'Polygons are then cleaned up using `polygons_clean()`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param mutually_exclusive_with list of `sf` object(s). This may be used to
#'ensure that polygons (e.g. parks, beaches, informal nature areas) are mutually-exclusive (i.e. non-overlapping).
#'Remove output polygons [contained](https://postgis.net/docs/ST_Contains.html) within, as well as [intersections](https://postgis.net/docs/ST_Intersection.html) between,
#'each element of this list. Should have the same coordinate reference system as `place`.
#'@param snap_tolerance numeric. Argument for `tolerance` level passed to `sf::st_snap()`,
#'used to rectify nearly coincident edges between polygons before processing (e.g. `sf::st_contains()`, `sf::st_covers()`).
#'Provided either as a units object (see `units::set_units()`), or a number in the units of the coordinate reference system.
#'Defaults to `5`. Set to `0` if you do not wish to rectify minor overlaps.
#'@param min_area numeric. Specify minimum area of each polygon to be retained in the output,
#'passed to argument `threshold` in `smoothr::drop_crumbs()`.
#'Provided either as a units object (see `units::set_units()`), or a number in the units of
#'the coordinate reference system. Defaults to `0` m^2.
#'@param aggregate_polygons numeric. Argument for `dist` passed to `sf::st_buffer()`.
#'Buffered polygons that overlap will be aggregated into multipolygons.
#'Set to `NULL` if you do not wish to aggregate to multipolygons.
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed beach polygons (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter mutate
#'@importFrom units set_units
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp)
#'
#' # merge all census blocks for chosen year (2020) into single multi-polygon
#' # function requires that polygons are merged
#' city_boundaries <- pop_sgp %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, 'km^2'))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, 'km^2'))  %>%
#'    sf::st_make_valid()
#'
#' data(parks_sgp) # to exclude beaches within/intersecting these polygons
#'
#' # run function
#' get_beaches_osm(place = city_boundaries,
#'                 date = as.Date('2021-01-01'),
#'                 mutually_exclusive_with = list(parks_sgp),
#'                 snap_tolerance = 5,
#'                 aggregate_polygons = 15,
#'                 filename = 'public-beaches_osm-polygons_2021-01-01.geojson')
#' }
#'
#'@export
get_beaches_osm <- function(place, date = NULL, mutually_exclusive_with = list(), snap_tolerance = 5,
    min_area = units::set_units(0, "m^2"), aggregate_polygons = 15, dir_raw = osmextract::oe_download_directory(),
    filename = NULL, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_date(date, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = TRUE,
        add = coll)
    checkmate::assert_list(mutually_exclusive_with, any.missing = FALSE, all.missing = TRUE,
        unique = TRUE, null.ok = FALSE, add = coll)

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    # all crs similar
    if (length(mutually_exclusive_with) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(mutually_exclusive_with, sf::st_crs), function(x) x ==
            sf::st_crs(place)))))
    }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # form url link ----
    link <- osmextract::oe_match(place, provider = "geofabrik")$url  # download region

    if (!is.null(date)) {
        date <- format(date, "%y%m%d")
        link <- gsub("latest", date, link)  # change download date if provided
    }


    # parameters to filter data after download ----
    osmkeys <- c("access")  # 'natural' already a default key

    q <- "SELECT * FROM 'multipolygons' WHERE natural = 'beach' AND (access IS NULL OR access NOT IN ('no', 'private'))"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, sf::st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()


    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = q, wkt_filter = bb)

    # transform back to same crs as 'place', then remove empty geoms, cast to individual
    # polygons
    suppressWarnings(results <- results %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))


    # run helper functions ----
    if (length(mutually_exclusive_with) != 0) {

        for (i in seq_along(mutually_exclusive_with)) {

            results <- polygons_mutually_exclude(input = results, mutually_exclude = mutually_exclusive_with[[i]],
                snap_tolerance = snap_tolerance)
        }
    }

    results <- polygons_clean(results, snap_tolerance = snap_tolerance, min_area = min_area,
        aggregate_polygons = aggregate_polygons)


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}

#'Get informal nature areas from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) 'informal nature' polygons within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM polygons are filtered by key-value attributes, where `landuse:` is `forest` or `meadow`,
#'or `natural:` is `wood`, `scrub`, `heath`, `grassland`, `wetland`, `marsh`, `fell` or `tundra`,
#'and `access:` is not `no`, `private` or `restricted`.
#'Polygons [contained](https://postgis.net/docs/ST_Contains.html) within `leisure:golf_course` are excluded after mutually-[snapping](https://postgis.net/docs/ST_Snap.html) vertices between the two
#'(see argument `snap_tolerance`). [Intersections](https://postgis.net/docs/ST_Intersection.html) with `landuse:military` polygons are also removed.
#'To exclude nature areas that are inaccessible, OSM trail lines are extracted (`highway:` is `track`, `path`, `footway` or `cycleway`,
#'and `access:` is not `no` or `private`). The remaining polygons without such trails [within](https://postgis.net/docs/ST_Within.html) their boundaries are removed.
#'If `mutually_exclusive_with` is provided, [intersections](https://postgis.net/docs/ST_Intersection.html) between
#'the output and these polygon(s) will be excluded using `polygons_mutually_exclude()`.
#'Polygons are then cleaned up using `polygons_clean()`.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param mutually_exclusive_with list of `sf` object(s). This may be used to
#'ensure that polygons (e.g. parks, beaches, informal nature areas) are mutually-exclusive (i.e. non-overlapping).
#'Remove output polygons [contained](https://postgis.net/docs/ST_Contains.html) within, as well as [intersections](https://postgis.net/docs/ST_Intersection.html) between,
#'each element of this list. Should have the same coordinate reference system as `place`.
#'@param snap_tolerance numeric. Argument for `tolerance` level passed to `sf::st_snap()`,
#'used to rectify nearly coincident edges between polygons before processing (e.g. `sf::st_contains()`, `sf::st_covers()`).
#'Provided either as a units object (see `units::set_units()`), or a number in the units of the coordinate reference system.
#'Defaults to `5`. Set to `0` if you do not wish to rectify minor overlaps.
#'@param min_area numeric. Specify minimum area of each polygon to be retained in the output,
#'passed to argument `threshold` in `smoothr::drop_crumbs()`.
#'Provided either as a units object (see `units::set_units()`), or a number in the units of
#'the coordinate reference system. Defaults to `0` m^2.
#'@param min_trails numeric. Specify minimum length of OSM trail lines that has to be within
#'nature area polygons, for the polygons to be retained in the output.
#'Provided either as a units object (see `units::set_units()`), or a number in the units of
#'the coordinate reference system. Defaults to `0` m.
#'@param aggregate_polygons numeric. Argument for `dist` passed to `sf::st_buffer()`.
#'Buffered polygons that overlap will be aggregated into multipolygons.
#'Set to `NULL` if you do not wish to aggregate to multipolygons.
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed 'informal nature areas' (polygons) that are publicly accessible (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@import osmextract
#'@importFrom dplyr filter mutate group_by summarise left_join
#'@importFrom units set_units
#'@importFrom smoothr fill_holes drop_crumbs
#'@importFrom rlang .data
#'
#'@examples
#' \dontrun{
#' data(pop_sgp)
#'
#' # merge all census blocks for chosen year (2020) into single multi-polygon
#' # function requires that polygons are merged
#' city_boundaries <- pop_sgp %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, 'km^2'))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, 'km^2'))  %>%
#'    sf::st_make_valid()
#'
#' data(parks_sgp) # to exclude nature areas within/intersecting these polygons
#'
#' # run function
#' get_informalnature_osm(place = city_boundaries,
#'                        date = as.Date('2021-01-01'),
#'                        mutually_exclusive_with = list(parks_sgp),
#'                        snap_tolerance = 5,
#'                        min_area = units::set_units(2500, 'm^2'),
#'                        min_trails = units::set_units(0, 'm'),
#'                        aggregate_polygons = 15,
#'                        filename = 'nature-areas_osm-polygons_2021-01-01.geojson')
#' }
#'
#'@export
get_informalnature_osm <- function(place, date = NULL, mutually_exclusive_with = list(), snap_tolerance = 5,
    min_area = units::set_units(0, "m^2"), min_trails = units::set_units(0, "m"), aggregate_polygons = 15,
    dir_raw = osmextract::oe_download_directory(), filename = NULL, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_date(date, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = TRUE,
        add = coll)
    checkmate::assert_list(mutually_exclusive_with, any.missing = FALSE, all.missing = TRUE,
        unique = TRUE, null.ok = FALSE, add = coll)

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE, add = coll)

    # all crs similar
    if (length(mutually_exclusive_with) != 0) {
        checkmate::assertTRUE(all(unlist(lapply(lapply(mutually_exclusive_with, sf::st_crs), function(x) x ==
            sf::st_crs(place)))))
    }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # form url link ----
    link <- osmextract::oe_match(place, provider = "geofabrik")$url  # download region

    if (!is.null(date)) {
        date <- format(date, "%y%m%d")
        link <- gsub("latest", date, link)  # change download date if provided
    }


    # parameters to filter data after download ----
    osmkeys <- c("access")  # 'landuse' & 'natural' already a default keys

    q <- "SELECT * FROM 'multipolygons' WHERE (landuse IN ('forest', 'meadow') OR natural IN ('wood', 'scrub', 'heath', 'grassland', 'wetland', 'marsh', 'fell', 'tundra')) AND (access IS NULL OR access NOT IN ('no', 'private', 'restricted'))"

    # to extract features that intersect bounding box (geographic crs)
    bb <- sf::st_transform(place, sf::st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()


    # download and filter data ---- filter by st_read() instead of using
    # vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = q, wkt_filter = bb)

    # transform back to same crs as 'place', then remove empty geoms, cast to individual
    # polygons
    suppressWarnings(results <- results %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))


    # exclude golf courses ----
    golf <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = "SELECT * FROM 'multipolygons' WHERE leisure = 'golf_course'",
        wkt_filter = bb)
    suppressWarnings(golf <- golf %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))

    # rectify minor overlaps w mutual snap, BUT SAVE TO NEW VAR
    golf2 <- st_snap(golf, results, tolerance = snap_tolerance)
    results2 <- st_snap(results, golf2, tolerance = snap_tolerance)

    # exclude entire polygon if contained within golf course (USE EDITED results2 variable)
    results <- results %>%
        dplyr::mutate(id = row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(st_contains(golf2, results2 %>%
            mutate(id = row_number()))))) %>%
        dplyr::select(-.data$id) %>%
        st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON")
    rm(results2, golf, golf2)


    # exclude military areas ----
    military <- osmextract::oe_read(link, download_directory = dir_raw, layer = "multipolygons",
        extra_tags = osmkeys, force_vectortranslate = TRUE, query = "SELECT * FROM 'multipolygons' WHERE landuse = 'military'",
        wkt_filter = bb)
    suppressWarnings(military <- military %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))

    # rectify minor overlaps w mutual snap
    military <- st_snap(military, results, tolerance = snap_tolerance)
    results <- st_snap(results, military, tolerance = snap_tolerance)

    # exclude intersection between the 2 polygons
    military <- military %>%
        st_union() %>%
        st_make_valid()
    results <- st_difference(results, military) %>%
        st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON")
    rm(military)


    # exclude polygons with no public trails ----
    trails <- osmextract::oe_read(link, download_directory = dir_raw, layer = "lines", extra_tags = c("access"),
        force_vectortranslate = TRUE, query = "SELECT * FROM 'lines' WHERE (highway IN ('track', 'path', 'footway', 'cycleway')) AND (access IS NULL OR access NOT IN ('no', 'private'))",
        wkt_filter = bb)
    suppressWarnings(trails <- trails %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid())

    results <- results %>%
        dplyr::mutate(id = row_number()) %>%
        dplyr::filter((.data$id %in% unlist(st_within(trails, results %>%
            mutate(id = row_number())))))

    # exclude nature areas below min trail length
    trails <- st_intersection(trails, results)  # subset trails
    trails$length <- st_length(trails)  # new col for line length

    trails <- trails %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarise(trails = sum(.data$length, na.rm = TRUE))
    st_geometry(trails) <- NULL

    results <- results %>%
        dplyr::left_join(trails, by = "id")

    results$trails[is.na(results$trails)] <- 0  # replace na with 0

    results <- results %>%
        dplyr::filter(!.data$trails < min_trails)
    rm(trails)


    # run helper functions ----
    if (length(mutually_exclusive_with) != 0) {

        for (i in seq_along(mutually_exclusive_with)) {

            results <- polygons_mutually_exclude(input = results, mutually_exclude = mutually_exclusive_with[[i]],
                snap_tolerance = snap_tolerance)
        }
    }

    results <- polygons_clean(results, snap_tolerance = snap_tolerance, min_area = min_area,
        aggregate_polygons = aggregate_polygons)


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE, overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}
