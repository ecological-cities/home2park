#'Match the year between the dataset of interest and another data source
#'
#'Helper function to match the year in the data with either the (1) exact;
#'(2) most recent (past); (3) closest (past or future); or (4) soonest (future) year in another dataset.
#'The column name within both datasets that represent the year must be specified.
#'Dataset containing multiple years must be in the 'long' format.
#'
#'@param data Tabular object (e.g. `data.frame`, `sf`, `tibble`) containing the data.
#'@param data_tomatch Tabular object (e.g. `data.frame`, `sf`, `tibble`) to match the data to.
#'@param match Type of matching; either `'exact'`, `'closest'`, `'recent'` or `'soonest'`.
#'@param year Specify column name for the year within both datasets.
#'Columns in both datasets should be numeric.
#'
#'@return `data` with additional column `year_match`,
#'representing the matching year of the other dataset.
#'
#'@import sf
#'@import checkmate
#'
#'@export
matchyear <- function(data, data_tomatch, match = c("exact", "closest", "recent", "soonest"), 
    year = NULL) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # colnames
    checkmate::assert_subset(year, choices = colnames(data), empty.ok = TRUE, add = coll)
    checkmate::assert_subset(year, choices = colnames(data_tomatch), empty.ok = TRUE, add = coll)

    # type of matching
    checkmate::assert_choice(match, choices = c("exact", "closest", "recent", "soonest"), 
        null.ok = FALSE, add = coll)

    # data type
    checkmate::assert_numeric(data[[year]], lower = 0, finite = TRUE, any.missing = FALSE, 
        all.missing = FALSE, null.ok = FALSE, add = coll)
    checkmate::assert_numeric(data_tomatch[[year]], lower = 0, finite = TRUE, any.missing = FALSE, 
        all.missing = FALSE, null.ok = FALSE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------
    years_data <- unique(data[[year]])
    years_otherdata <- unique(data_tomatch[[year]])

    data$year_match <- NA  # add empty col representing matching landuse year

    for (i in seq_along(years_data)) {

        if (match == "exact") {

            year_match <- years_otherdata[years_otherdata == years_data[i]]
            if (length(year_match) == 0) {
                year_match <- NA
            }

            # append data if not empty
            data$year_match[data$year == years_data[i]] <- year_match

            rm(year_match)

        } else if (match == "closest") {

            year_match <- years_otherdata[which.min(abs(years_otherdata - years_data[i]))]  # index no for closest yr
            if (length(year_match) == 0) {
                year_match <- NA
            }

            # append data if not empty
            data$year_match[data$year == years_data[i]] <- year_match

            rm(year_match)

        } else if (match == "recent") {

            which_after <- which(years_otherdata <= years_data[i])  # other data BEFORE data
            years_otherdata_after <- years_otherdata[which_after]

            year_match <- years_otherdata_after[which.min(abs(years_otherdata_after - years_data[i]))]  # index no for closest yr in other data
            if (length(year_match) == 0) {
                year_match <- NA
            }

            # append data if not empty
            data$year_match[data$year == years_data[i]] <- year_match

            rm(which_after, years_otherdata_after, year_match)

        } else if (match == "soonest") {

            which_before <- which(years_otherdata >= years_data[i])  # other data AFTER data
            years_otherdata_before <- years_otherdata[which_before]

            year_match <- years_otherdata_before[which.min(abs(years_otherdata_before - years_data[i]))]  # index no for soonest yr in other data
            if (length(year_match) == 0) {
                year_match <- NA
            }

            # append data if not empty
            data$year_match[data$year == years_data[i]] <- year_match

            rm(which_before, years_otherdata_before, year_match)
        }

        rm(i)
    }

    return(data)
}
