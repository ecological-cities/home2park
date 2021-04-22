#'Calculate cumulative supply of a park attribute per building polygon
#'
#'The supply $S$ of the park attribute is calculated based on the following equation:
#'
#'1) \eqn{S = \sum\limits_{i=1}^{n} s_{i} \cdot e^{-cd_{i}}}
#'
#' where
#'
#' \eqn{S} = Total supply of a specific park attribute to the building from parks \eqn{i};
#' \eqn{i = 1,2,3}... \eqn{n}, \eqn{n} = total number of parks
#'
#' \eqn{s_{i}} = Supply of a specific park attribute from park \eqn{i}.
#' A perfect positive linear association is assumed, since the focus is on supply metrics.
#' Other coefficients (e.g.\eqn{ks^{z}_{i}}) may be added to model (monotonically positive)
#' relationships with actual park use or downstream benefits.
#'
#' \eqn{d_{i}} = Euclidean distance from the building to park \eqn{i}.
#'
#' \eqn{c} = Coefficient determining rate of decay in supply \eqn{i} with increasing distance.
#'
#'@param park_attribute Vector of quantities of a specific attribute for all parks.
#'Length of vector is equal to the number of parks considered.
#'@param dist_matrix Matrix containing buildings (rows) and their pairwise distances to each park (columns).
#'Order of parks (columns) should be identical to the order of parks (elements) in `park_attribute`.
#'@param c Coefficient determining rate of decay in recreation supply with increasing distance
#'
#'@return A vector containing the cumulative supply values across all parks,
#'for each building (row) in the input matrix `dist_matrix`.
#'The length of the vector equals to the number of buildings considered.
#'
#'@examples
#'
#'
#'@import checkmate
#'@importFrom units set_units
#'
#'@export
ss_recre_perbuilding <- function(park_attribute, dist_matrix, c = 1) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # colnames
    checkmate::assert_numeric(park_attribute, lower = 0, finite = TRUE, all.missing = FALSE, 
        len = ncol(dist_matrix), add = coll)
    checkmate::assert_matrix(dist_matrix, mode = "numeric", all.missing = FALSE, ncols = length(park_attribute), 
        add = coll)
    checkmate::assert_number(c, lower = 0, upper = Inf, finite = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # remove units park_attribute <- units::drop_units(park_attribute) dist_matrix <-
    # units::drop_units(dist_matrix)
    park_attribute <- units::set_units(park_attribute, value = NULL)  # can still run if already no units
    dist_matrix <- units::set_units(dist_matrix, value = NULL)

    # convert vector of park area to matrix for matrix calculations park_attribute <-
    # scale(park_attribute, center = FALSE) # scale before making into matrix w duplicated
    # values!
    attribute_matrix <- matrix(park_attribute, nrow = nrow(dist_matrix), ncol = length(park_attribute), 
        byrow = TRUE)


    # calculate recre supply Dist: proportional (linear) inverse relationship # supply <-
    # attribute_matrix * 1/(scale(dist_matrix + 0.001, center = FALSE)) # scale before #
    # supply <- attribute_matrix * scale(1/(dist_matrix + 0.001), center = FALSE) # scale
    # before (scale reciprocal of dist) supply <- scale(attribute_matrix * 1/(dist_matrix +
    # 0.001), center = FALSE) # scale after

    # Dist: decay exponentially
    supply <- attribute_matrix * exp(-c * dist_matrix)  # don't scale (w coefficient)
    # supply <- attribute_matrix * exp(-scale(dist_matrix, center = FALSE)) # scale before #
    # supply <- attribute_matrix * exp(-c * scale(dist_matrix, center = FALSE)) # scale before
    # (w coefficient) # supply <- scale(attribute_matrix * exp(-dist_matrix), center = FALSE)
    # # scale after

    supply <- apply(supply, 1, sum)  # sum per row


    return(supply)
}

