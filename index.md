
<!-- README.md is generated from README.Rmd. Please edit that file -->

# home2park: Spatial Provision of Urban Parks

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/ecological-cities/home2park/workflows/R-CMD-check/badge.svg)](https://github.com/ecological-cities/home2park/actions)
[![test-coverage](https://github.com/ecological-cities/home2park/workflows/test-coverage/badge.svg)](https://github.com/ecological-cities/home2park/actions)
<!-- badges: end -->

<a href='https://ecological-cities.github.io/home2park/'><img src='man/figures/logo.png' align="right" height="175" /></a>

`home2park` is an R package for assessing the spatial provision of urban
parks to residential buildings city-wide. Refer to the [package
website](https://ecological-cities.github.io/home2park/) for
demonstrations of how the package may be used.

## Installation

Install the development version of `home2park` from GitHub:

``` r
devtools::install_github("ecological-cities/home2park")
```

## Setup

Load the package:

``` r
library(home2park)
```

## Citation

To cite `home2park` or acknowledge its use, please cite the following:

*Song, X. P., Chong, K. Y., home2park: An R package to assess the
spatial provision of urban parks (in prep).*

<br>

The get a BibTex entry, run `citation("home2park")`.

<br>

## Background

Parks are important spaces for recreation and leisure in cities.
Conventional metrics that measure the provision of parks to city
residents usually summarise the park area within a given region
(e.g. per capita park area). However, there is a need to characterise
the wide variety of parks (e.g. nature areas, gardens, waterfront parks,
outdoor playgrounds, etc.) that serve different groups of people. When
planning at fine spatial scales, such metrics are also limited by their
coarse spatial resolution and the presence of artificial boundaries.

<br>

## Using home2park

The package `home2park` provides a way to measure *multiple aspects* of
park provision to homes, at the resolution of *individual buildings*.
The key features include the ability to:

-   Download relevant data from OpenStreetMap (OSM), e.g., buildings,
    parks and features related to recreation/leisure (alternatively, the
    user may use their own datasets).
-   Redistribute coarse-scale population data (per census block region)
    into residential buildings, also known as ‘dasymetric mapping’.
-   Summarise multiple attributes that are important for
    recreation/leisure, at each park.
-   Calculate the supply (provision) of the park attributes to each
    residential building, while accounting for ‘distance decay’, or the
    fact that supply from parks further away are reduced.

The following sections provide a high-level overview of the various
steps required to measure the spatial provision of parks. Further
details and code examples can be found in the package vignette ‘[Get
started](articles/home2park.html)’

### 1. Process city population

Residential buildings (homes) are an important component of the
analysis. These may be obtained, for example, by downloading building
polygons from OpenStreetMap (OSM), and subsetting the dataset to areas
within ‘residential’ land use zones.

In addition, having the population count per residential building allows
us to calculate the total spatial provision of parks to all residents,
and can help highlight important areas where more people will benefit
from the presence of parks. Coarse-scale population census data can be
redistributed into the residential buildings, via a technique known as
‘dasymetric mapping’. The number of building ‘levels’ from OSM can be
used as a proxy for population density (i.e. more residents per unit
area). Here’s an example screenshot showing an overlay of multiple
example datasets in the package (for the city of Singapore), which were
used to redistribute population data per census block (subzones) across
residential buildings.

<div class="figure" style="text-align: center">

<img src="man/figures/README-dasymetric-mapping.png" alt="Screenshot: Residential population in Singapore redistributed across buildings within residential land use zones. The legends are ordered (top to bottom) by increasing spatial resolution." width="80%" />
<p class="caption">
Screenshot: Residential population in Singapore redistributed across
buildings within residential land use zones. The legends are ordered
(top to bottom) by increasing spatial resolution.
</p>

</div>

<br>

Residential building polygons in Singapore each with a population count
can be found in the following example dataset:

``` r
data(buildings_pop_sgp)
head(buildings_pop_sgp)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 368430.2 ymin: 161460 xmax: 370940.2 ymax: 161610
#> Projected CRS: WGS 84 / UTM zone 48N
#>     popcount                       geometry
#> 1 218.885435 POLYGON ((368490.2 161610, ...
#> 2  39.797352 POLYGON ((368430.2 161550, ...
#> 3   9.219688 POLYGON ((370680.2 161550, ...
#> 4   9.949338 POLYGON ((368460.2 161530, ...
#> 5  85.282118 POLYGON ((370840.2 161550, ...
#> 6 218.885435 POLYGON ((368460.2 161580, ...
```

<br>

### 2. Process parks

Parks are the other important component of the analysis. These may be
downloaded and processed from OSM using this package. The following
example dataset contains parks in Singapore with selected attributes
related to recreation/leisure:

``` r
data(parks_sgp)
head(parks_sgp[, 28:33]) # subset to relevant columns
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 364364.4 ymin: 138034.6 xmax: 374328.5 ymax: 142157.4
#> Projected CRS: WGS 84 / UTM zone 48N
#>               area     perimeter playground_count playground_ptdensity
#> 1   2454.365 [m^2]  346.5265 [m]                0            0 [1/m^2]
#> 2   1964.286 [m^2]  305.3844 [m]                0            0 [1/m^2]
#> 3 219319.203 [m^2] 3019.2241 [m]                0            0 [1/m^2]
#> 4  22513.834 [m^2]  615.1039 [m]                0            0 [1/m^2]
#> 5 571259.766 [m^2] 3973.5567 [m]                0            0 [1/m^2]
#> 6  67533.345 [m^2] 1092.2758 [m]                0            0 [1/m^2]
#>    trails_length trails_length_perim_ratio                       geometry
#> 1     0.0000 [m]             0.0000000 [1] MULTIPOLYGON (((371727.8 13...
#> 2     0.0000 [m]             0.0000000 [1] MULTIPOLYGON (((371454.2 13...
#> 3  4072.5481 [m]             1.3488724 [1] MULTIPOLYGON (((367154.1 13...
#> 4   837.9212 [m]             1.3622434 [1] MULTIPOLYGON (((369087 1409...
#> 5 20947.6579 [m]             5.2717652 [1] MULTIPOLYGON (((373309.8 14...
#> 6   905.5080 [m]             0.8290105 [1] MULTIPOLYGON (((364400.3 14...
```

<br>

### 3. Recreation supply

With the processed building and park polygons, the provision of park
attributes per residential building can be calculated. The total supply
*S* of each park attribute to a building is calculated based on the
following equation. Its value depends on the distances between that
particular building and all parks; attributes from parks further away
are generally reduced as a result of the negative exponential function
*e<sup>-cd</sup>*, an effect also known as the ‘distance decay’ ([Rossi
et al., 2015](http://dx.doi.org/10.1016/j.apgeog.2015.06.008)).

<img src="man/figures/README-equation.png" width="18%" style="display: block; margin: auto;" />

where

-   *S* = Total supply of a specific park attribute to the building from
    parks *i*; *i* = 1,2,3, … *n* where *n* = total number of parks
    citywide.

-   *s*<sub>*i*</sub> = Supply of a specific park attribute from park
    *i*. A perfect positive linear association is assumed, since the
    focus is on supply metrics. If park area has been calculated as an
    attribute, other attributes may be calculated as relative amounts
    (e.g. point density, ratio of line-to-perimeter length, proportional
    area) to avoid double-counting the effect of park size.

-   *d*<sub>*i*</sub> = Euclidean distance from the building to park
    *i*.

-   *c* = Coefficient determining rate of decay in supply
    *s*<sub>*i*</sub> with increasing distance.

<br>

Note that the value of Coefficient *c* depends on both park and park
visitors’ attributes, such as socio-demographic factors and preferences
for activities that may impel shorter or longer travel ([Rossi et al.,
2015](http://dx.doi.org/10.1016/j.apgeog.2015.06.008); [Tu et
al.](https://doi.org/10.1016/j.ufug.2020.126689)). A lower value implies
that parks further away are accessible or frequently visited by
residents (i.e. still contributes to the ‘recreation supply’ of a
particular building).

<div class="figure" style="text-align: center">

<img src="man/figures/README-c-sensitivity.png" alt="Figure: The effect of Coefficient c and its effect on the distance decay between a building and park." width="50%" />
<p class="caption">
Figure: The effect of Coefficient c and its effect on the distance decay
between a building and park.
</p>

</div>

<br>

<div class="figure" style="text-align: center">

<img src="man/figures/README-c-sensitivity-map.png" alt="Screenshot: The supply of park area to residential buildings in Singapore when the value of Coefficient c is 0.0001 (left panel) and 0.01 (right panel). Singapore measures approximately 50 km east to west, and the largest parks/nature areas tend to be centrally located. The color palette for the buildings (points) are binned according to their quantile values." width="100%" />
<p class="caption">
Screenshot: The supply of park area to residential buildings in
Singapore when the value of Coefficient c is 0.0001 (left panel) and
0.01 (right panel). Singapore measures approximately 50 km east to west,
and the largest parks/nature areas tend to be centrally located. The
color palette for the buildings (points) are binned according to their
quantile values.
</p>

</div>

<br>

To calculate the supply of each park attribute, we first calculate the
pairwise distances between all buildings and parks (a distance matrix).
This is supplied to the function `recre_supply()`. For example, we can
calculate the supply of park *area* to each building. This supply value
can then be multiplied by the population count per building, to obtain
the total supply to all residents.

``` r
# convert buildings to points (centroids), then calculate distances to every park
m_dist <- buildings_pop_sgp %>%
  sf::st_centroid() %>%
  sf::st_distance(parks_sgp) %>%
    units::set_units(NULL)


# new column for the supply of park area
buildings_pop_sgp$area_supply <- recre_supply(park_attribute = parks_sgp$area, 
                                              dist_matrix = m_dist, 
                                              c = 0.302) # e.g. from Tu et al. (2020)

# supply to all residents per building
buildings_pop_sgp$area_supplytopop <- buildings_pop_sgp$area_supply * buildings_pop_sgp$popcount



# scale the supply variables if necessary
buildings_pop_sgp <- buildings_pop_sgp %>%
    dplyr::mutate(across(.cols = c(area_supply, area_supplytopop), 
                         ~(scale(., center = FALSE))))
```

<div class="figure" style="text-align: center">

<img src="man/figures/README-supply-parkarea-to-building-residents.png" alt="Screenshot: Supply of park area to building residents in Singapore based on OSM data (2020). The color palette were binned according to quantile values." width="100%" />
<p class="caption">
Screenshot: Supply of park area to building residents in Singapore based
on OSM data (2020). The color palette were binned according to quantile
values.
</p>

</div>

<br>

## Data sources

-   Singapore census data from the [Department of Statistics
    Singapore](https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data).
    Released under the terms of the [Singapore Open Data Licence version
    1.0](https://data.gov.sg/open-data-licence).

-   Singapore census block (subzone) polygons from the [Singapore Master
    Plan
    Subzones](https://data.gov.sg/dataset/master-plan-2019-subzone-boundary-no-sea).
    Released under the terms of the [Singapore Open Data Licence version
    1.0](https://data.gov.sg/open-data-licence).

-   Singapore Master Plan Land Use Zones for the years
    [2014](https://data.gov.sg/dataset/master-plan-2014-land-use) and
    [2019](https://data.gov.sg/dataset/master-plan-2019-land-use-layer).
    Released under the terms of the [Singapore Open Data
    License](https://data.gov.sg/open-data-licence).

-   Building polygons derived from map data
    [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap
    contributors and available from <https://www.openstreetmap.org>.
    Released under the terms of the [ODbL
    License](https://opendatacommons.org/licenses/odbl/summary/).

-   Park polygons and summarised attributes (trails, playgrounds)
    derived from map data
    [copyrighted](https://www.openstreetmap.org/copyright) OpenStreetMap
    contributors and available from <https://www.openstreetmap.org>.
    Released under the terms of the [ODbL
    License](https://opendatacommons.org/licenses/odbl/summary/).

<br>

## References

Rossi, S. D., Byrne, J. A., & Pickering, C. M. (2015). The role of
distance in peri-urban national park use: Who visits them and how far do
they travel?. Applied Geography, 63, 77-88.

Tu, X., Huang, G., Wu, J., & Guo, X. (2020). How do travel distance and
park size influence urban park visits?. Urban Forestry & Urban Greening,
52, 126689.
