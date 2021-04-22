#'Make polygon input mutually exclusive (no overlaps) with another
#'
#'Helper function to ensure that polygons (e.g. parks, beaches, informal nature areas)
#'are mutually-exclusive (i.e. non-overlapping).
#'
#'Polygons vertices first are [snapped](https://postgis.net/docs/ST_Snap.html) at a tolerance level
#'based on `snap_tolerance` (if specified) to rectify nearly coincident edges,
#'before removing `input` polygons [contained](https://postgis.net/docs/ST_Contains.html) in `mutually_exclude`.
#'[Intersections](https://postgis.net/docs/ST_Intersection.html) between the two are then removed.
#'
#'@param input `sf` object to process (with projected coordinate reference system).
#'@param mutually_exclude `sf` object. `input` polygons [contained](https://postgis.net/docs/ST_Contains.html)
#'within this object, and [intersections](https://postgis.net/docs/ST_Intersection.html) between the two, will be removed.
#'Should have the same coordinate reference system as `input`.
#'@param snap_tolerance numeric. Argument for `tolerance` level passed to `sf::st_snap()`,
#'used to rectify nearly coincident edges between polygons before running `sf::st_contains()`.
#'Provided either as a units object (see `units::set_units()`),
#'or a number in the units of the coordinate reference system. Defaults to `0`.
#'
#'@return The processed `input` polygons (`sf` object).
#'
#'@import sf
#'@import checkmate
#'@importFrom dplyr filter mutate select row_number
#'@importFrom rlang .data
#'
polygons_mutually_exclude <- function(input, mutually_exclude, snap_tolerance = 0) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    checkmate::assertTRUE(!st_is_longlat(input) & !is.null(st_crs(input)), add = coll)  # must be projected crs
    checkmate::assertTRUE(st_crs(input) == st_crs(mutually_exclude))
    checkmate::assert_number(snap_tolerance, lower = 0, null.ok = FALSE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # remove those contained within rectify minor overlaps first
    input2 <- st_snap(input, mutually_exclude, tolerance = snap_tolerance)
    mutually_exclude2 <- st_snap(mutually_exclude, input2, tolerance = snap_tolerance)  # mutual snap, but SAVE TO NEW VAR

    input2 <- st_make_valid(input2) %>%
        st_cast("POLYGON")
    mutually_exclude2 <- st_make_valid(mutually_exclude2) %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON")

    # then remove those contained based on edited var
    input <- input %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(!(.data$id %in% unlist(st_contains(mutually_exclude2, input2 %>%
            mutate(id = dplyr::row_number()))))) %>%
        dplyr::select(-.data$id)
    rm(input2, mutually_exclude2)


    # remove intersections
    toremove <- mutually_exclude %>%
        st_union() %>%
        st_make_valid()

    input <- st_difference(input, toremove) %>%
        st_make_valid() %>%
        st_cast("MULTIPOLYGON") %>%
        st_cast("POLYGON")

    rm(toremove)


    return(input)
}
