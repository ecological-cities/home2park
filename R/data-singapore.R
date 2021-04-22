#' Population counts within census block polygons in Singapore
#'
#' Example dataset containing Singapore census data for years 2019 and 2020 from the [Department of Statistics Singapore](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data),
#'joined by name to the Master Plan Subzone polygons from [data.gov.sg](https://data.gov.sg/dataset/master-plan-2019-subzone-boundary-no-sea).
#' Both datasets are released under the [Singapore Open Data License](https://data.gov.sg/open-data-licence).
#'
#' @docType data
#'
#' @format `sf` polygons with census data for years 2019 and 2020.
#' Data is in the 'long' format (rows are repeated for each census year).
#' The following columns are used in this package:
#' \describe{
#'   \item{subzone_n}{Census block name}
#'   \item{year}{Census year}
#'   \item{pop_count}{Population count within census block}
#'  }
#'
#' @keywords datasets
#'
#' @source
#' Contains census data from the [Department of Statistics Singapore](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data)
#' and polygons from the [Singapore Master Plan Subzones](https://data.gov.sg/dataset/master-plan-2019-subzone-boundary-no-sea),
#' both of which are made available under the terms of the [Singapore Open Data Licence version 1.0](https://data.gov.sg/open-data-licence).
#'
#' @examples
#' data(singapore)
#' head(singapore)
#'
#' \dontrun{
#' # city boundaries - combine all census blocks for a specific year, then simplify
#' city_boundaries <- data(singapore) %>%
#'    dplyr::filter(year == 2020) %>%
#'    sf::st_union() %>%
#'    sf::st_as_sf() %>%
#'    smoothr::fill_holes(threshold = units::set_units(1, "km^2"))  %>%
#'    smoothr::drop_crumbs(threshold = units::set_units(1, "km^2"))  %>%
#'    sf::st_make_valid()
#' }
"singapore"

#' Land use master plan for the city of Singapore
#'
#' Example dataset containing Singapore Master Plan Land Use Zones for the years
#' [2014](https://data.gov.sg/dataset/master-plan-2014-land-use) and
#' [2019](https://data.gov.sg/dataset/master-plan-2019-land-use-layer).
#' Released under the [Singapore Open Data License](https://data.gov.sg/open-data-licence).
#'
#' @docType data
#'
#' @format `sf` polygons of land use released in years 2014 and 2019.
#' Data is in the 'long' format (rows are repeated for each year).
#' The following columns are used in this package:
#' \describe{
#'   \item{lu_desc}{Land use type (description)}
#'   \item{year}{Year of Master Plan}
#'  }
#'
#' @keywords datasets
#'
#' @source
#' Polygons from the Singapore Land Use Master Plans released in the years
#' [2014](https://data.gov.sg/dataset/master-plan-2014-land-use) and
#' [2019](https://data.gov.sg/dataset/master-plan-2019-land-use-layer).
#' Made available under the terms of the [Singapore Open Data Licence version 1.0](https://data.gov.sg/open-data-licence).
#'
#' @examples
#' data(landuse_sgp)
#' head(landuse_sgp)
"landuse_sgp"

#' Buildings in Singapore
#'
#' Example dataset of building polygons in Singapore downloaded from OpenStreetMap
#' (data snapshot on `2021-01-01` from the from the [Geofabrik database](https://download.geofabrik.de)),
#' using the function `get_buildings_osm()`.
#'
#' @docType data
#'
#' @format `sf` polygons. The following columns are used in this package:
#' \describe{
#'   \item{levels}{Number of building levels, derived from `building_levels`.
#'   Values were set to `1` if the extracted value was empty or `NA`, and set to `NA` if `≤ 0` (i.e. underground);
#'   values were then rounded up to the nearest whole number.}
#'  }
#'
#' @keywords datasets
#'
#' @source
#' Map data [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap contributors and available from https://www.openstreetmap.org.
#'
#' @examples
#' data(buildings_sgp)
#' head(buildings_sgp)
"buildings_sgp"

#' Public parks in Singapore
#'
#' Example dataset of park polygons in Singapore downloaded from OpenStreetMap
#' (data snapshot on `2021-01-01` from the from the [Geofabrik database](https://download.geofabrik.de)),
#' using the function `get_parks_osm()`.
#'
#' @docType data
#'
#' @format `sf` polygons.
#'
#' @keywords datasets
#'
#' @source
#' Map data [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap contributors and available from https://www.openstreetmap.org.
#'
#' @examples
#' data(parks_sgp)
#' head(parks_sgp)
"parks_sgp"
