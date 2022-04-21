#' Get a buffer object for Switzerland
#'
#' @param gdf A spatial data frame of class sf with geometry "POLYGON" representing the countries of the world.
#' @param SWISS_BUFFER Numeric. Distance in meters to Swiss border, under which the area is interpreted as close-to Swizerland.
#'
#' @return A named list with two elements (chdf_buffer and chdf_3035 sf objects objects with geometry "POLYGON" representing Swizerland and its proximity, enlarged by a buffer).
#'
get_chdf_buffer <- function(gdf, SWISS_BUFFER) {

  gdf_3035 <- sf::st_transform(gdf, crs = 3035)
  chdf <- gdf[gdf$ISO_A3 == "CHE", ]
  chdf_3035 <- gdf_3035[gdf_3035$ISO_A3 == "CHE", ]
  chdf_3035_buffer <- sf::st_difference(sf::st_buffer(chdf_3035, SWISS_BUFFER), chdf_3035)
  chdf_3035_buffer$ISO_CHbuffer <- 1
  chdf_3035_buffer$ISO_A3 <- NULL
  chdf_3035_buffer$ISO_A3.1 <- NULL
  chdf_buffer <- sf::st_transform(chdf_3035_buffer, crs = sf::st_crs(gdf))

  return(list(chdf_buffer = chdf_buffer, chdf_3035 = chdf_3035))
}
