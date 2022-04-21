#' Replace missing values with defaults where applicable
#'
#' @param features Data frame with missing values.
#' @param MEDIANthreshold Threshold in percent: If the number of missings abount to a share of less than this value, NAs will be replaces with median. Default is 0.05.
#' @return The feature data frame where missing values have been replaced.
#'
fill_default_values <- function(features, MEDIANthreshold = 5) {

    # replace NaN and infinite values with NA
  for (f in 1:dim(features)[2]) {
    features[is.nan(features[[f]]), f] <- NA
    features[is.infinite(features[[f]]), f] <- NA
  }

  # prepare a list of variables, for which 0 is a more appropriate default than NA for a missing value replacement
  nanames_zerorep <- NULL

  #explicit naming
  nanames_zerorep <- c("total_activity","total_activity_tagged","total_activity_median", "time_higher_night_if_free")

  #all features that involve a count
  nanames_zerorep <- c(nanames_zerorep, names(features)[grepl("count$", names(features))])
  nanames_zerorep <- c(nanames_zerorep, names(features)[grepl("count_", names(features))])

  #all features that involve a percent
  nanames_zerorep <- c(nanames_zerorep, names(features)[grepl("_pct", names(features))])

  #all features on spending and mobility
  nanames_zerorep <- c(nanames_zerorep, names(features)[grepl("*spend_", names(features))])
  nanames_zerorep <- c(nanames_zerorep, names(features)[grepl("*mob_", names(features))])

  #exclude doublicates and non-numerics
  nanames_zerorep <- unique(nanames_zerorep)
  nanames_zerorep <- nanames_zerorep[!nanames_zerorep %in% names(unlist(lapply(features, is.numeric))[unlist(lapply(features, is.numeric)) == F])]

  # replace NA with 0 in the variables selected
  features[nanames_zerorep][is.na(features[nanames_zerorep])] <- 0

  # replace NA with median if number of missings is under MEDIANthreshold
  for (f in 1:dim(features)[2]) {
    if ((is.numeric(features[[f]])) & (base::sum(is.na(features[[f]]))/length(features[[f]]) < (MEDIANthreshold/100))) {
      features[is.na(features[[f]]), f] <- stats::median(features[[f]], na.rm = T)
    }
  }

  return(features)
}
