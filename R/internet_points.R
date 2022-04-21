#' Extract all transactions that likely took place via the internet
#'
#' @param df_raw Data frame containing coordinate and businesstype columns.
#' @param LAT_COLUMN String. Name of the column of df containing the latitude of location.
#' @param LON_COLUMN String. Name of the column of df containing the longitude of location.
#'
#' @return A geometry object (class sf) containing points at which internet transactions took place.
#'
create_internetpoints <- function(df_raw, LAT_COLUMN, LON_COLUMN) {

  # Subset df_raw to transactions across the internet and exclude
  # observations with missing coordinates.
  df_sp_i <- df_raw[df_raw$businesstype == "internet", ]
  df_sp_i[, LAT_COLUMN] <- as.numeric(df_sp_i[, LAT_COLUMN])
  df_sp_i[, LON_COLUMN] <- as.numeric(df_sp_i[, LON_COLUMN])
  df_sp_i <- df_sp_i[!is.na(df_sp_i[, LAT_COLUMN]) & !is.na(df_sp_i[, LON_COLUMN]), ]

  # Create spatial points
  points_geomi <- sf::st_cast(
    x = sf::st_sfc(sf::st_multipoint(
      cbind(df_sp_i[, LON_COLUMN], df_sp_i[, LAT_COLUMN])
    )),
    to = "POINT"
  )

  # Put data together in spatial format
  internetpoints <- sf::st_sf(
    geometry = points_geomi,
    crs = 4326
  )

  return(internetpoints)
}
