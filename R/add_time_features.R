
#' Extract time features
#'
#' @param customer_df The transaction data frame containing only records associated with a single customer.
#' @param customer_id THe ID of the single customer associated with customer_df.
#'
#' @return customer_df with additional columns.
#'
get_features <- function(customer_df, customer_id) {

  # Initialize columns
  entry <- data.frame(id = customer_id)
  entry$time_median <- NaN
  entry$time_early <- NaN
  entry$time_late <- NaN
  entry$time_most_common_daytime_free <- NaN
  entry$time_most_common_daytime_work <- NaN
  entry$time_pct_free <- NaN
  entry$time_count_night <- NaN
  entry$time_pct_night <- NaN
  entry$time_pct_night_free <- NaN
  entry$time_pct_night_work <- NaN
  entry$time_higher_night_if_free <- NaN

  # Most common daytime
  df_tmp <- customer_df[customer_df$today_freeday, ]
  most_common <- names(sort(
    table(df_tmp$time_of_day),
    decreasing = TRUE
  )[1])
  if (!is.null(most_common)) {
    entry$time_most_common_daytime_free <- most_common
  }

  df_tmp <- customer_df[!customer_df$today_freeday, ]
  most_common <- names(sort(
    table(df_tmp$time_of_day),
    decreasing = TRUE
  )[1])
  if (!is.null(most_common)) {
    entry$time_most_common_daytime_work <- most_common
  }

  # Work vs free
  entry$time_pct_free <- base::sum(customer_df$today_freeday)/nrow(customer_df)

  entry$time_late <- stats::quantile(
    ifelse(
      customer_df$nighttimes > 5,
      customer_df$nighttimes - 24,
      customer_df$nighttimes
    ),
    probs = 9/10,
    names = F,
    na.rm = T
  )
  entry$time_late <- ifelse(
    entry$time_late < 0,
    entry$time_late + 24,
    entry$time_late
  )

  entry$time_early <- stats::quantile(customer_df$daytimes, probs = 1/10, names = F, na.rm = T)
  entry$time_median <- stats::quantile(customer_df$daytimes, probs = 5/10, names = F, na.rm = T)

  # Day vs night
  df_night <- customer_df[customer_df$day_or_night == "night", ]
  entry$time_count_night <- nrow(df_night)
  entry$time_pct_night <- nrow(df_night)/nrow(customer_df)

  df_tmp_free <- customer_df[customer_df$tomorrow_freeday, ]
  df_tmp_free_night <- df_tmp_free[df_tmp_free$day_or_night == "night",]
  entry$time_pct_night_free <- nrow(df_tmp_free_night)/nrow(df_tmp_free)

  df_tmp_work <- customer_df[!customer_df$tomorrow_freeday, ]
  df_tmp_work_night <- df_tmp_work[df_tmp_work$day_or_night == "night", ]
  entry$time_pct_night_work <- nrow(df_tmp_work_night)/nrow(df_tmp_work)

  norm <- entry$time_pct_night_free + entry$time_pct_night_work
  entry$time_higher_night_if_free <- (entry$time_pct_night_free - entry$time_pct_night_work)/norm

  return(entry)
}


#' Check if a given day was free of work
#'
#' @param dt Chracter vector. The date time.
#' @return A boolean character of the same length as dt.
#'
evaluate_today_freeday <- function(dt) {
  # freeday = is.weekend(dt)
  bizday = timeDate::isBizday(
    timeDate::as.timeDate(dt),
    holidays = timeDate::holidayZURICH(2000:2021))
  freeday = as.vector(!bizday)
  return(freeday)
}


#' Check if the following day of a given day was free of work
#'
#' @param dt Chracter vector. The date time.
#' @return A boolean character of the same length as dt.
#' @export
#'
evaluate_tomorrow_freeday <- function(dt) {
  tomorrow <- dt + 60*60*24
  bizday = timeDate::isBizday(
    timeDate::as.timeDate(tomorrow),
    holidays = timeDate::holidayZURICH(2000:2021))
  freeday = as.vector(!bizday)
  return(freeday)
}


#' Add time features to a feature collection
#'
#' @param features A data frame to which new features will be added.
#' @param df A data frame containing the columns specified by the following arguments.
#' @param CUSTOMER_ID_COLUMN String. Name of the column of df containing the customer identifier.
#' @param TIME_COLUMN String. Name of the column of df containing timestamps.
#'
#' @return The features data frame with additional columns.
#'
add_time_features <- function(features, df, CUSTOMER_ID_COLUMN, TIME_COLUMN) {

  # Assign time features
  df_features <- df[CUSTOMER_ID_COLUMN]

  df_features$date <- as.POSIXct(df[, TIME_COLUMN], origin = "1970-01-01") #format = "%Y-%m-%d",

  df_features$hour <- as.integer(format(df_features$date, format = "%H"))

  df_features$today_freeday <- evaluate_today_freeday(df_features$date)

  df_features$tomorrow_freeday <- evaluate_tomorrow_freeday(df_features$date)

  df_features$time_of_day <- as.character(cut(
    df_features$hour,
    breaks = c(-Inf, 4, 9, 12, 18, 22, Inf),
    labels = c("night", "early_morning", "early_day", "afternoon", "evening", "night")
  ))

  df_features$day_or_night <- as.character(cut(
    df_features$hour,
    breaks = c(-Inf, 4, 22, Inf),
    labels = c("night", "day", "night")
  ))

  df_features$nighttimes <- ifelse(
    df_features$day_or_night == "night",
    df_features$hour,
    NA
  )

  df_features$daytimes <- ifelse(
    df_features$day_or_night == "day",
    df_features$hour,
    NA
  )

  customer_ids <- unique(df[CUSTOMER_ID_COLUMN])[, 1]
  for (i in c(customer_ids)) {
    customer_df <- df_features[df_features[, CUSTOMER_ID_COLUMN] == i, ]
    customer_features <- get_features(customer_df, i)
    features <- rbind(features, customer_features)
  }

  return(features)
}
