#' Create sf object of card transaction data and compute geographic/mobility related information
#'
#' Add some features related to distance (centroids per user) and area (space covered by user transactions).
#' @param df_sp A spatial data frame of class sf with geometry "POINT" representing transaction data.
#' @param gdf A spatial data frame of class sf with geometry "POLYGON" representing the countries of the world.
#' @param internetpoints A geometry object of class sf containing points at which internet transactions took place.
#' @param chdf_buffer  sf objects objects with geometry "POLYGON" representing Swizerland and its proximity, enlarged by a buffer.
#' @param chdf_3035 sf objects objects with geometry "POLYGON" in crs with unit=meter representing Swizerland and its proximity, enlarged by a buffer.
#' @param SWISS_BUFFER Numeric. Distance in meters to Swiss border, under which the area is interpreted as close-to Swizerland.
#'
#' @return The sdf_sp data frame with additional columns.
#'
get_sf_cardpoints <- function(df_sp, gdf, internetpoints, chdf_buffer, chdf_3035, SWISS_BUFFER) {

  # Remove internetpoints
  sf_cardpoints <- sf::st_difference(df_sp, sf::st_combine(internetpoints))

  sf_cardmultipoints <- aggregate(
    sf_cardpoints,
    by = list(sf_cardpoints$id),
    FUN = base::mean
  )
  sf_cardmultipoints$Group.1 <- NULL

  # Add country codes to sf_cardpoints
  sf_cardpoints <- sf::st_join(
    sf_cardpoints, gdf,
    join = sf::st_intersects
  )

  # Add indicator for whether point lies in buffer
  sf_cardpoints <- sf::st_join(
    sf_cardpoints, chdf_buffer,
    join = sf::st_intersects
  )

  #### Distance-related features.

  # Determine centroids for each user
  # (centre point of all transaction points associated with the user)
  sf_cardpoints_centroids <- sf::st_as_sf(
    base::merge(
      data.frame("id" = sf_cardpoints$id),
      sf::st_cast(
        sf::st_centroid(sf_cardmultipoints),
        to = "POINT"
      )[1],
      all.y = F,
      all.x = T,
      by = "id"
    )
  )

  # Calculate feature:
  # Distance to previous points
  sf_cardpoints$distance_to_previous <- geodist::geodist(
    sf::st_coordinates(sf_cardpoints),
    sequential = T,
    pad = T
  )
  sf_cardpoints$distance_to_previous <- ifelse(
    sf_cardpoints$id != c("first", sf_cardpoints[1:(nrow(sf_cardpoints) - 1), ]$id),
    NA,
    sf_cardpoints$distance_to_previous
  )

  # Calculate feature:
  # Distance to centroids
  sf_cardpoints$distance_to_centroid <- geodist::geodist(
    sf::st_coordinates(sf_cardpoints),
    sf::st_coordinates(sf_cardpoints_centroids),
    paired = T
  )

  #### Area-related features
  ##### Area of the "transaction/mobility space" (convex hull of all points) of a user
  ##### divided by total area of Switzerland is used as indicator for "spatial dispersion".
  ##### The resulting index is called "mob_pct_of_che".

  # Transform to CRS 3035 (more suitable to area calculations than WGS 85)
  sf_cardpoints_3035 <- sf::st_transform(sf_cardpoints, crs = 3035)
  sf_cardmultipoints_3035 <- sf::st_transform(sf_cardmultipoints, crs = 3035)

  # Calculate cardpoints in spatial format.
  # Remove points in buffer
  sf_cardmultipoints_3035ch <- sf::st_intersection(
    sf_cardmultipoints_3035,
    sf::st_buffer(chdf_3035, SWISS_BUFFER)
  )
  sf_cardmultipoints_3035ch$mob_pct_of_che <- as.numeric(sf::st_area(
      sf::st_convex_hull(sf_cardmultipoints_3035ch)
    ) / sf::st_area(chdf_3035)
  )
  sf_cardmultipoints_3035ch <- sf_cardmultipoints_3035ch[, c("id", "mob_pct_of_che")]
  sf::st_geometry(sf_cardmultipoints_3035ch) <- NULL
  sf_cardpoints <- merge(
    sf_cardpoints,
    sf_cardmultipoints_3035ch,
    all.x = T,
    all.y = F,
    by = "id"
  )

  # Calculate area
  sf_cardmultipoints_3035$area <- as.numeric(sf::st_area(
    sf::st_convex_hull(sf_cardmultipoints_3035)
  ))
  sf_cardmultipoints_3035 <- sf_cardmultipoints_3035[, c("id", "area" )]
  sf::st_geometry(sf_cardmultipoints_3035) <- NULL
  sf_cardpoints <- base::merge(
    sf_cardpoints,
    sf_cardmultipoints_3035,
    all.x = T,
    all.y = F,
    by = "id"
  )

  return(sf_cardpoints)
}


#' Add columns with statistics on transactions in foreign countries
#'
#' @param sf_cardpoints A geometry object of class sf containing points at which internet transactions took place.
#'
#' @return sf_cardpoints with additional columns.
#'
add_foreign_countries <- function(sf_cardpoints) {

  # Calculate points outside of CH
  sf_cardpoints$nonCH <- ifelse(sf_cardpoints$ISO_A3 == "CHE", 0, 1)
  sf_cardpoints$ISO_CHbuffer <- ifelse(is.na(sf_cardpoints$ISO_CHbuffer), 0, 1)

  # Aggregate data for non-CH states
  ForeignCountryStats <- data.table::data.table(
    sf_cardpoints[sf_cardpoints$nonCH == 1, ])[, .N, by = .(id, ISO_A3)][order(-N), .(
      Favorit_Country = data.table::first(ISO_A3),
      Favorit_Country_N = base::max(N),
      Country_N = .N),
      by = id]

  # Add foreign information to data
  sf_cardpoints <- base::merge(
    sf_cardpoints,
    ForeignCountryStats,
    all.x = T,
    all.y = F,
    by = "id"
  )

  return(sf_cardpoints)
}
