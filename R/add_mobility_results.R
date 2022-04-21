#' Add mobility results
#'
#' @param features A data frame to which new features will be added.
#' @param sf_cardpoints geocoded transaction data stored in spatial data frame of class sf with geometry "POINT".
#'
#' @return The features data frame with additional columns.
#'
add_mobility_results <- function(features, sf_cardpoints) {

  # Combine all mobility features
  mobility_results <- data.table::data.table(sf_cardpoints)[, .(
    mob_mean_distance_to_previous = stats::median(distance_to_previous, na.rm = T),
    mob_std_distance_to_previous  = stats::sd(distance_to_previous, na.rm = T),
    mob_mean_distance_to_center   = stats::median(distance_to_centroid, na.rm = T),
    mob_std_distance_to_center    = stats::sd(distance_to_centroid, na.rm = T),
    mob_hull_circle_ratio         = base::max(distance_to_centroid, na.rm = T)^2 * pi / base::max(area),
    mob_hullarea_per_t            = base::max(area) / .N,
    mob_pct_of_che                = base::mean(mob_pct_of_che, na.rm = T),
    mob_farfrom_switzerland       = base::mean(nonCH, na.rm = T) - base::mean(ISO_CHbuffer, na.rm = T),
    mob_closeto_switzerland       = base::mean(ISO_CHbuffer, na.rm = T),
    mob_country_count_different   = (Country_N[1]),
    mob_country_most_visited      = (base::ifelse(!is.na(Favorit_Country[1]), Favorit_Country[1], 'no_foreign_country_trx')),
    mob_country_most_visited_pct  = base::mean(Favorit_Country_N) / .N),
    by = id]

  # Add mobility features to previous features
  features <- base::merge(
    x = features,
    y = mobility_results,
    all.x = T,
    all.y = F,
    by = "id"
  )


  return(features)
}
