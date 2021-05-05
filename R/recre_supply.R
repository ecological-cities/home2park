#' Calculate cumulative supply of an attribute from all parks to each building
#'
#' Sum up the total amount of a specific park attribute supplied to each building polygon.
#' The amount per building depends on the distances between that particular building and all parks;
#' attributes from parks further away are generally reduced, an effect also known as
#' the 'distance decay' (Rossi et al., 2015; Tu et al., 2020).
#'
#' The supply \eqn{S} of the park attribute is calculated based on the following equation:
#'
#'  \eqn{S = \sum\limits_{i=1}^{n} s_{i} \cdot e^{-cd_{i}}}
#'
#'  where
#'
#'  \eqn{S} = Total supply of a specific park attribute to the building from parks \eqn{i};
#'  \eqn{i = 1,2,3}... \eqn{n}, \eqn{n} = total number of parks
#'
#'  \eqn{s_{i}} = Supply of a specific park attribute from park \eqn{i}.
#'  A perfect positive linear association is assumed, since the focus is on supply metrics.
#'  Other coefficients (e.g.\eqn{ks^{z}_{i}}) could be added to future versions of this package,
#'  in order to model (monotonically positive) relationships with actual park use or downstream benefits.
#'
#'  \eqn{d_{i}} = Euclidean distance from the building to park \eqn{i}.
#'
#'  \eqn{c} = Coefficient determining rate of decay in supply \eqn{i} with increasing distance.
#'
#' @param park_attribute numeric vector. Amount of a specific attribute per park.
#' Length of vector is equal to the number of parks considered.
#' @param dist_matrix Matrix containing buildings (rows) and their pairwise distances to each park (columns).
#' Order of parks (columns) should be identical to the order of parks (elements) in `park_attribute`.
#' @param c Coefficient determining rate of decay in recreation supply with increasing distance.
#'
#' @return A numeric vector of the cumulative supply value per building (row) in the input matrix `dist_matrix`.
#' The length of the vector equals to the number of buildings considered.
#'
#' @source
#' Rossi, S. D., Byrne, J. A., & Pickering, C. M. (2015). The role of distance in peri-urban national park use:
#' Who visits them and how far do they travel?. Applied Geography, 63, 77-88.
#'
#' Tu, X., Huang, G., Wu, J., & Guo, X. (2020). How do travel distance and park size influence urban park visits?.
#' Urban Forestry & Urban Greening, 52, 126689.
#'
#' @examples
#' \dontrun{
#'   data(parks_sgp) # load park polygons
#'   data(buildings_pop_sgp) # load building polygons w population counts
#'
#'
#'   # Calculate pairwise distances between (the centroid of) each building & all parks
#'   d_matrix <- buildings_pop_sgp %>%
#'     st_centroid() %>%
#'     st_distance(parks_sgp)
#'
#'
#'  # run function for a specific park attribute (e.g. area)
#'   recre_supply(park_attribute = parks_sgp$area,
#'                dist_matrix = d_matrix,
#'                c = 0.3) # example value for distance decay coefficient c
#'
#' }
#'
#'@import checkmate
#'@importFrom units set_units
#'
#'@export
recre_supply <- function(park_attribute, dist_matrix, c = 1){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # colnames
  checkmate::assert_numeric(park_attribute, lower = 0, finite = TRUE, all.missing = FALSE, len = ncol(dist_matrix), add = coll)
  checkmate::assert_matrix(dist_matrix, mode = "numeric", all.missing = FALSE, ncols = length(park_attribute), add = coll)
  checkmate::assert_number(c, lower = 0, upper = Inf, finite = TRUE, add = coll)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # remove units
  park_attribute <- units::set_units(park_attribute, value = NULL) # can still run if already no units
  dist_matrix <- units::set_units(dist_matrix, value = NULL)

  # convert vector of park attribute to matrix for matrix calculations
  attribute_matrix <- matrix(park_attribute, nrow=nrow(dist_matrix), ncol = length(park_attribute), byrow = TRUE)


  # calculate recre supply
  # supply <-  attribute_matrix * 1/(dist_matrix + 0.001), center = FALSE) # proportional (linear) inverse relationship w distance
  supply <- attribute_matrix * exp(-c * dist_matrix) # negative exponential relationship w dist


  supply <- apply(supply, 1, sum) # sum per row


  return(supply)
}

