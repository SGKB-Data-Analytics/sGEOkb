#' Add spending results
#'
#' @param features A data frame to which new features will be added.
#' @param df_raw A data frame containing transaction data.
#' @param GOOGLE_COLUMN String. Name of the column of df containing business type (from Google API).
#' @param AMOUNT_COLUMN String. Name of the column of df containing the amount.
#' @param CUSTOMER_ID_COLUMN String. Name of the column of df containing the customer identifier.
#'
#' @return The features data frame with additional columns.
#'
add_spending_results <- function(features,
                                 df_raw,
                                 GOOGLE_COLUMN,
                                 AMOUNT_COLUMN,
                                 CUSTOMER_ID_COLUMN) {

  spending_results <- data.table::data.table(df_raw)[, .(
    # colnames(df_raw)

    # Activities
    total_activity = .N,

    # Activities by type
    total_activity_tagged = base::sum(
      base::get(GOOGLE_COLUMN) != "NULL" &
        !is.null(base::get(GOOGLE_COLUMN)) &
        !is.na(base::get(GOOGLE_COLUMN))),


    # Aggregated activities
    total_activity_median = stats::median(
      as.numeric(base::get(AMOUNT_COLUMN)), na.rm = T),

    price_sensitivity = base::mean(
      as.numeric(pricestype), na.rm = T),

    price_sensitivity_sd = stats::sd(
      as.numeric(pricestype), na.rm = T)
  ),

  # Group by
  by = CUSTOMER_ID_COLUMN]

  # Initialize businesstypes
  businesstypes <- unique(df_raw$businesstype)

  # Create features based on businesstype
  for (bt in c(businesstypes)) {

    df_bt_sub <- df_raw[df_raw$businesstype == bt, ]

    # Grociers/Restaurants/Gas
    if (bt == "groceries" | bt == "restaurant" | bt == "gas" | bt == "shopping") {

      # Naming
      name_count <- paste0("spend_", bt, "_count")
      name_pct <- paste0("spend_", bt, "_pct")
      name_q3 <- paste0("spend_", bt, "_q66minusq33")
      name_q6 <- paste0("spend_" ,bt, "_q66")

      # Aggregate by quantiles
      dt_bt_sub <- data.table::data.table(df_bt_sub)[, .(
        xcount = .N,
        xpct = .N,
        q3 = stats::quantile(as.numeric(base::get(AMOUNT_COLUMN)), probs = 1/3, names = F, na.rm = T) -
          stats::quantile(as.numeric(base::get(AMOUNT_COLUMN)), probs = 2/3, names = F, na.rm = T),
        q6 = stats::quantile(as.numeric(base::get(AMOUNT_COLUMN)), probs = 1/3, names = F, na.rm = T)),
        by = CUSTOMER_ID_COLUMN]

      # Assign names
      data.table::setnames(
        dt_bt_sub,
        c(CUSTOMER_ID_COLUMN, name_count, name_pct, name_q3, name_q6)
      )

      # Combine data
      spending_results <- dt_bt_sub[spending_results, on = CUSTOMER_ID_COLUMN]
      spending_results[, name_pct] <- spending_results[, base::get(name_pct)] / spending_results[, total_activity]
      rm(name_count, name_pct, name_q3,name_q6, dt_bt_sub)


    } else {
      # Other businesstypes

      # Naming
      name_count <- paste0("spend_", bt, "_count")
      name_pct <- paste0("spend_", bt, "_pct")
      name_median <- paste0("spend_", bt, "_median")

      # Aggregate
      dt_bt_sub <- data.table::data.table(df_bt_sub)[, .(
        xcount = .N,
        xpct = .N,
        xmedian = stats::median(as.numeric(base::get(AMOUNT_COLUMN)), na.rm = T)),
        by = CUSTOMER_ID_COLUMN]

      # Assign names
      data.table::setnames(
        dt_bt_sub,
        c(CUSTOMER_ID_COLUMN, name_count, name_pct, name_median)
      )

      # Combine data
      spending_results <- dt_bt_sub[spending_results, on = CUSTOMER_ID_COLUMN]
      spending_results[, name_pct] <- spending_results[, base::get(name_pct)]/spending_results[, total_activity]
      rm(name_count, name_pct, name_median, dt_bt_sub)
    }
  }

  spending_results[, base::get(CUSTOMER_ID_COLUMN)][is.na(spending_results$price_sensitivity)]

  # Add  features to time & raster features
  features <- base::merge(
    x = features,
    y = spending_results,
    all.x = T,
    all.y = F,
    by.x = "id",
    by.y = CUSTOMER_ID_COLUMN
  )

  return(features)
}
