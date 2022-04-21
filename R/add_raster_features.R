
#' Extract raster values from given coordinates
#'
#' @param gpd_coords_spdf A spatial data frame of class sf with geometry "POINT", for which raster values will be extracted.
#' @param RASTERSTACK_PATH String. Path to directory containing raster files.
#' @param RASTERS Character vector. Names of raster files (without file-extension) that should be used.
#'
#' @return gpd_coords_spdf with additional columsn representing features from the raster files.
#'
get_rasterstack_features <- function(gpd_coords_spdf, RASTERSTACK_PATH, RASTERS){

  # Identify rasters to keep
  lista_variables <- list.files(path = RASTERSTACK_PATH , pattern = '*.tif$', full.names = TRUE)
  lista_variables <- lista_variables[grepl(x = lista_variables, pattern = paste(RASTERS, collapse = "|"))]

  # Stack rasters
  variable_stack <- raster::stack(lista_variables)

  # Extract raster data
  extractdata <- raster::extract(
    variable_stack,
    sf::st_coordinates(gpd_coords_spdf),
    method = "bilinear"
  )
  colnames(extractdata) <- names(variable_stack)

  # Extend spdf with new features
  gpd_coords_spdf <- cbind(
    gpd_coords_spdf,
    extractdata
  )

  return(gpd_coords_spdf)
}


#' Aggregate and add raster features
#'
#'
#' @param features A data frame to which new features will be added.
#' @param df_sp A spatial data frame of class sf with geometry "POINT", for which raster values will be extracted - and aggregated by its id.
#' @param RASTERS Character vector. Names of raster files (without file-extension).
#' @param RASTER_FILTERED String. Path to directory containing raster files.
#'
#' @return The features data frame with additional columns.
#'
add_raster_features <- function(features, df_sp, RASTERS, RASTERSTACK_PATH) {

  # Validate raster files
  if (!validate_raster_files(RASTERSTACK_PATH, RASTERS)) {
    return(NULL)
  }

  # Log debug information about used raster files
  futile.logger::flog.debug("Looking for raster files at %s", RASTERSTACK_PATH)
  futile.logger::flog.debug(
    "Using raster files:\n%s",
    paste(RASTERS, collapse = "\n")
  )

  # Add feature values
  coords_spdf <- get_rasterstack_features(df_sp, RASTERSTACK_PATH, RASTERS)

  # Double check rasters existent in data
  RASTER_FILTERED <- RASTERS[RASTERS %in% colnames(coords_spdf)]

  # Extract aggregated value for each feature (median)
  geo_results <- data.table::data.table(
    coords_spdf)[, lapply(.SD, stats::median, na.rm = T),
                 .SDcols = RASTER_FILTERED, by = id][
                   , data.table::setnames(
                     .SD, RASTER_FILTERED,
                     paste0("geo_", tolower(RASTER_FILTERED)))]

  # Extract aggregated value for each feature (sd)
  geo_results_sd <- data.table::data.table(
    coords_spdf)[,lapply(.SD, sd, na.rm = T),
                 .SDcols = RASTER_FILTERED, by = id][
                   , data.table::setnames(
                     .SD, RASTER_FILTERED,
                     paste0("geo_", tolower(RASTER_FILTERED),"_sd"))]

  geo_results <- cbind(geo_results, geo_results_sd[, -1])
  geo_results <- data.frame(geo_results)[, order(names(geo_results))]

  # Adjust Hoehe if existent
  if ("Geography_Hight" %in% RASTER_FILTERED) {

    # Assign max height
    geo_results$geo_geography_hight <- data.table::data.table(
      coords_spdf)[,lapply(.SD, max, na.rm = T),
                   .SDcols = "Geography_Hight",
                   by = id]$Geography_Hight
  }

  # Combine features
  features <- base::merge(
    x = features,
    y = geo_results,
    all.x = T,
    all.y = F,
    by = "id"
  )

  return(features)
}
