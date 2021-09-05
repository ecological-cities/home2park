#' Population counts within census block polygons in Singapore
#'
#' Example dataset containing Singapore census data for years 2018 and 2020 from the [Department of Statistics Singapore](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data),
#'joined by name to the Master Plan Subzone polygons from [data.gov.sg](https://data.gov.sg/dataset/master-plan-2019-subzone-boundary-no-sea).
#' Both datasets are released under the [Singapore Open Data License](https://data.gov.sg/open-data-licence).
#'
#' @docType data
#'
#' @format `sf` polygons with census data for years 2018 and 2020.
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
#' data(pop_sgp)
#' head(pop_sgp)
"pop_sgp"

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

#' Population count per residential building in Singapore
#'
#' Example (random 5% subset) dataset of residential building polygons in Singapore,
#' each with a population count (column `popcount`)
#' estimated via dasymetric mapping.
#'
#' Building polygons were downloaded from OpenStreetMap
#' (data snapshot on `2021-01-01` from the [Geofabrik database](https://download.geofabrik.de)),
#' using the function `get_buildings_osm()`. The population count per census block in the year 2020
#' was re-distributed across the buildings located within residential land use zones,
#' by performing dasymetric mapping using the functions `pop_dasymap()` and `pop_density_polygonise()`.
#' See vignette and examples in `pop_density_polygonise()` for more details.
#' The dataset is a random 5% subset of the resulting polygons.
#'
#' @docType data
#'
#' @format `sf` polygons.
#'
#' @keywords datasets
#'
#' @source
#' Building polygons [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap contributors and available from https://www.openstreetmap.org.
#' Made available via the [ODbL License](https://opendatacommons.org/licenses/odbl/summary/).
#'
#' Population data and census block polygons `data(pop_sgp)` are from the [Department of Statistics Singapore](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data);
#' and [Singapore Master Plan Subzones](https://data.gov.sg/dataset/master-plan-2019-subzone-boundary-no-sea), respectively;
#' land use polygons `data(landuse_sgp)` are from the Singapore Land Use Master Plan released in [2019](https://data.gov.sg/dataset/master-plan-2019-land-use-layer).
#' All are made available under the terms of the [Singapore Open Data Licence version 1.0](https://data.gov.sg/open-data-licence).
#'
#' @examples
#' data(buildings_pop_sgp)
#' head(buildings_pop_sgp)
"buildings_pop_sgp"

#' Public parks in Singapore
#'
#' Example dataset of park polygons in Singapore downloaded from OpenStreetMap
#' (data snapshot on `2021-01-01` from the [Geofabrik database](https://download.geofabrik.de)),
#' using the function `get_parks_osm()`. Includes summaries (columns) of selected attributes related
#' to outdoor recreation, i.e., playgrounds (see `get_playgrounds_osm()`) and trails (see `get_trails_osm()`)
#'calculated using the function `parks_calc_attributes()`.
#'
#' @docType data
#'
#' @format `sf` polygons.
#'
#' @keywords datasets
#'
#' @source
#' Map data [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap contributors and available from https://www.openstreetmap.org.
#' Made available via the [ODbL License](https://opendatacommons.org/licenses/odbl/summary/).
#'
#' @examples
#' data(parks_sgp)
#' head(parks_sgp)
"parks_sgp"
