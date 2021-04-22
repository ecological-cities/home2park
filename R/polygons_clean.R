#'Clean up polygons after download
#'
#'Helper function to process and clean up polygons downloaded from OpenStreetMap.
#'
#'Polygons [contained](https://postgis.net/docs/ST_Contains.html) in or [overlapping](https://postgis.net/docs/ST_Overlaps.html) others are removed.
#'Polygons vertices are then [snapped](https://postgis.net/docs/ST_Snap.html)
#'at a tolerance level based on `snap_tolerance` (if specified) to rectify nearly coincident edges,
#'and polygons [covered](https://postgis.net/docs/ST_Covers.html) by others are subsequently removed. Polygons are then aggregated
#'to multipolygons based on distance set in `aggregate_polygons` (if specified).
#'All spatial relations (italicised) follow the [DE-9IM](https://en.wikipedia.org/wiki/DE-9IM) standard.
#'
#'@param input `sf` object (with projected coordinate reference system).
#'@param snap_tolerance numeric. Argument for `tolerance` level passed to `sf::st_snap()`,
#'used to rectify nearly coincident edges between polygons. Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system.
#'After `sf::st_snap()` is run, a polygon that is [covered](https://postgis.net/docs/ST_Covers.html) by another will be removed.
#'Defaults to `5`. Set to `0` if you do not wish to rectify minor overlaps.
#'@param min_area numeric. Specify minimum area of each polygon to be retained in the output,
#'passed to argument `threshold` in `smoothr::drop_crumbs()`.
#'Provided either as a units object (see `units::set_units()`), or a number in the units of
#'the coordinate reference system. Defaults to `0` \eqn{m^2}.
#'@param aggregate_polygons numeric. Argument for `dist` passed to `sf::st_buffer()`.
#'Buffered polygons that overlap will be aggregated into multipolygons.
#'Set to `NULL` if you do not wish to aggregate to multipolygons.
#'
#'@return The processed polygons (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@importFrom dplyr filter mutate select row_number na_if first
#'@importFrom tidyselect matches
#'@importFrom smoothr drop_crumbs
#'@importFrom units set_units drop_units
#'@importFrom rlang .data
#'
polygons_clean <- function(input, snap_tolerance = 5, min_area = units::set_units(0, "m^2"),
    aggregate_polygons = 15) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    checkmate::assert_number(snap_tolerance, lower = 0, null.ok = FALSE, add = coll)
    checkmate::assert_number(min_area, lower = 0, null.ok = FALSE, add = coll)
    checkmate::assert_number(aggregate_polygons, lower = 0, null.ok = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # polygon removal ---- remove polygons contained wholly within another
    results <- input %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(st_contains_properly(input)))) %>%
        dplyr::select(-.data$id)

    # remove polygons contained within another
    results <- results %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(lapply(st_contains(results), function(x) x[-1])))) %>%
        dplyr::select(-.data$id)

    # remove overlapping polygon that share some but not all interior points
    results <- results %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(lapply(st_overlaps(results), function(x) x[-1])))) %>%
        dplyr::select(-.data$id)


    # remove polygons covered by others rectify minor overlaps first (using .data doesn't work)
    results <- results %>%
        st_snap(x = ., y = ., tolerance = snap_tolerance)
    suppressWarnings(results <- st_make_valid(results) %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON"))
    # remove polygons covered by, but share some of the border
    results <- results %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(lapply(st_covers(results), function(x) x[-1])))) %>%
        dplyr::select(-.data$id) %>%
        dplyr::mutate(name = dplyr::na_if(.data$name, ""))  # replace empty string with NA


    # remove polygons < min area ----
    if (units::drop_units(min_area) > 0) {
        results <- smoothr::drop_crumbs(results, threshold = min_area)
    }


    # aggregate into multipolygons ----
    if (!is.null(aggregate_polygons)) {

        poly_buffered <- st_buffer(results, dist = aggregate_polygons)

        # get geom, divide into individual polygons (single part geom), then assign unique ID for
        # matching
        poly_buffered <- st_union(poly_buffered) %>%
            st_sf() %>%
            st_cast("POLYGON") %>%
            dplyr::mutate(id = dplyr::row_number())

        # join ID of buffered to original, based on whether they are within the buffered polygons, then make into individual polygons
        results <- st_join(results, poly_buffered, join = st_within) %>%
            st_cast("POLYGON")
        rm(poly_buffered)

        # create multipolygon based on new IDs, using first row's info
        results <- results %>%
            stats::aggregate(list(results$id), dplyr::first) %>%
            st_cast("MULTIPOLYGON")
    }


    # final name clean up ----
    results <- results %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::relocate(.data$id, .data$name) %>%
        dplyr::select(-tidyselect::matches("Group.1"))  # remove (if aggregated to multipolygons)

    rownames(results) <- NULL  # reset rownames


    return(results)
}
