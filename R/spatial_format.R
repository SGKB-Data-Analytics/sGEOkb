#' Transform data frame to spatial format
#'
#' This functions takes a data frame and extracts location information
#' (longitude and latitude columns) as well as a customer identifier
#' into a spatial data frame of class sf ('simple features collection'),
#' orderd by customer id and time.
#'
#' @param df A data frame containing the columns specified by the following arguments.
#' @param LAT_COLUMN String. Name of the column of df containing latitude values.
#' @param LON_COLUMN String. Name of the column of df containing longtitude values.
#' @param TIME_COLUMN  String. Name of the column of df containing timestamps.
#' @param CUSTOMER_ID_COLUMN  String. Name of the column of df containing the customer identifier.
#'
#' @return A spatial data frame of class sf
#'
transform_to_spatial_format <- function(df,
                                        LAT_COLUMN,
                                        LON_COLUMN,
                                        TIME_COLUMN,
                                        CUSTOMER_ID_COLUMN) {

  if (!is.data.frame(df)) {
    futile.logger::flog.error("transform_to_spatial_format expects a data frame df")
    return(NULL)
  }
  for (var in c(LAT_COLUMN, LON_COLUMN, TIME_COLUMN, CUSTOMER_ID_COLUMN)) {
    if (!var %in% colnames(df)) {
      futile.logger::flog.error("transform_to_spatial_format: %s is not a column of df", var)
      return(NULL)
    }
  }

  df_sp <- df
  df_sp[, LAT_COLUMN] <- as.numeric(df_sp[, LAT_COLUMN])
  df_sp[, LON_COLUMN] <- as.numeric(df_sp[, LON_COLUMN])
  df_sp <- df_sp[order(df_sp[, CUSTOMER_ID_COLUMN],
                       df_sp[, TIME_COLUMN]),]
  df_sp <- df_sp[!is.na(df_sp[, LAT_COLUMN]) & !is.na(df_sp[, LON_COLUMN]),]

  # Create spatial points
  points_geom <- sf::st_cast(
    x  = sf::st_sfc(sf::st_multipoint(cbind(df_sp[, LON_COLUMN], df_sp[, LAT_COLUMN]))),
    to = "POINT"
  )

  # Put data together in spatial format
  df_sp <- sf::st_sf(
    id = df_sp[, CUSTOMER_ID_COLUMN],
    geometry = points_geom,
    crs = 4326
  )
  return(df_sp)
}
