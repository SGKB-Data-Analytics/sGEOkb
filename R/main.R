
#' Compute features for input data
#'
#' @param args A named list containing the arguments as key-value pairs.
#' @param dev_mode Boolean. Run this function in development mode with test data
#' and verbose logging?
#'
#' @return A boolean indicating successful computation.
#' This function is called for its side effects. Results are written to a data base.
#' @export
#'
main <- function(args = NULL, dev_mode = FALSE) {

  # Use a progessbar to indicate progression through stages
  pb <- txtProgressBar(min = 0, max = 7, style = 3)
  cat("\n")

  # Validate arguments
  if (is.null(args)) {
    futile.logger::flog.error("Required argument 'args' is missing")
    return(FALSE)
  }
  if (!is.list(args)) {
    futile.logger::flog.error("'args' must be a list")
    return(FALSE)
  }
  if (length(args) > 0 && (is.null(names(args)) || any(is.na(names(args))))) {
    futile.logger::flog.error("'args' must be a named list. Missing values for names are not allowed.")
    return(FALSE)
  }
  if (!"connection_config" %in% names(args)) {
    args$connection_config <- "conn_config.yml"
  }

  # ________________________________________________________________

  ### 1. Settings ####
  # ________________________________________________________________

  # Load parameters
  if ("path_lib" %in% names(args)) {
    .libPaths(c(args$path_lib, .libPaths()))
    futile.logger::flog.debug("Using .libPaths %s", .libPaths())
  }

  if (!"parameters_file" %in% names(args)) {
    parameters_file <- system.file(package = "sGEOkb", "parameters.yaml")
    if (dev_mode) {
      parameters_file <- "tests/test_parameters.yaml"
    }
  } else {
    # parameters_file provided explicitly. It is not checked at this point if the file exists.
    parameters_file <- args$parameters_file
  }

  # Load and validate parameters
  params <- load_parameters(parameters_file)
  if (is.null(params)) {
    return(FALSE)
  }
  if (!validate_parameters(params)) {
    return(FALSE)
  }

  ############
  # Establish ODBC connection.
  # If not in dev-mode, assert that connection config file exists and is complete
  if (!dev_mode) {
    if (!file.exists(args$connection_config)) {
      futile.logger::flog.error("Connection config file for data base connection is missing")
      return(FALSE)
    }
    if (!validate_connection_config(args$connection_config)) {
      return(FALSE)
    }
  }

  odbc_conn <- odbc_connect(
    params$DATA_PATH_DB,
    dev_mode = dev_mode,
    connection_config = args$connection_config
  )
  if (is.null(odbc_conn)) {
    return(FALSE)
  }

  futile.logger::flog.info("Loaded parameters and connected to data base")
  setTxtProgressBar(pb, 1)
  cat("\n")

  # ________________________________________________________________

  ### 2. Data Import ####
  # ________________________________________________________________

  # Determine INPUTS_PATH which contains the mapping files and countries.geojson document.
  # If no path is passed with the call to main(), then fall back to defaults defined in inst/.
  if (!"INPUTS_PATH" %in% names(params)) {
    params$INPUTS_PATH <- system.file(package = "sGEOkb")
  }
  futile.logger::flog.debug("Using INPUTS_PATH %s", params$INPUTS_PATH)

  # Load and validate country polygons
  if ("COUNTRIES_GEOJSON_FILE" %in% names(params)) {
    countries_file <- file.path(params$COUNTRIES_GEOJSON_FILE)
  } else {
    countries_file <- file.path(params$INPUTS_PATH, "countries.geojson")
  }
  if (!file.exists(countries_file)) {
    futile.logger::flog.error("Could not find geojson-file containing country data")
    return(FALSE)
  }
  gdf <- geojsonsf::geojson_sf(countries_file)
  if (!validate_countries_data(gdf)) {
    return(FALSE)
  }

  # Load and validate mapping files
  mapping_files <- load_mapping_files(params$INPUTS_PATH)
  if (!validate_mapping_files(mapping_files)) {
    return(FALSE)
  }

  # Load and validate transaction data
  df <- DBI::dbGetQuery(conn = odbc_conn, statement = paste0("Select * from ", params$DATA_PATH_DB))
  if (!validate_transaction_data(df, params)) {
    return(FALSE)
  }

  # Transform transaction data to spatial format
  df_sp <- transform_to_spatial_format(
    df,
    params$LAT_COLUMN,
    params$LON_COLUMN,
    params$TIME_COLUMN,
    params$CUSTOMER_ID_COLUMN
  )
  if (is.null(df_sp)) {
    return(FALSE)
  }

  # Set up data structure to which new features will be added step by step.
  # The data contained in this data frame represent the output of all computations
  # and at the end of the main function, it will be exported.
  features <- data.frame()

  # All variables until here are the Stage. This serves for efficient memory management.
  futile.logger::flog.info("Loaded data from data base and transformed to spatial format")
  setTxtProgressBar(pb, 2)
  cat("\n")


  # ________________________________________________________________

  ### 3. Time Features ####
  # ________________________________________________________________

  features <- add_time_features(
    features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$TIME_COLUMN
  )

  futile.logger::flog.info("Added time features")
  setTxtProgressBar(pb, 3)
  cat("\n")

  # ______________________________________________________________________

  # 4. Raster Features ####
  # ______________________________________________________________________

  features <- add_raster_features(features, df_sp, params$RASTERS, params$RASTERSTACK_PATH)

  gc(verbose = FALSE) # Ensure that environment of last function call (add_raster_features) is deleted
  futile.logger::flog.info("Added raster features")
  setTxtProgressBar(pb, 4)
  cat("\n")

  # ______________________________________________________________________

  # 5. Spending Features ####
  # ______________________________________________________________________

  df_raw <- apply_mappings(
    df,
    mapping_files$BuchT_Mapping_json,
    mapping_files$GoogT_Mapping_json,
    mapping_files$BusiT_Mapping_json,
    mapping_files$Price_Mapping_json,
    params$LOCATION_COLUMN,
    params$ALT_LOCATION_COLUMN,
    params$BOOKING_COLUMN,
    params$GOOGLE_COLUMN
  )

  ### Aggregate results
  features <- add_spending_results(
    features,
    df_raw,
    params$GOOGLE_COLUMN,
    params$AMOUNT_COLUMN,
    params$CUSTOMER_ID_COLUMN
  )

  futile.logger::flog.info("Applied mappings and added spending results")
  setTxtProgressBar(pb, 5)
  cat("\n")

  # ______________________________________________________________________

  # 6. Mobility Features ####
  # ______________________________________________________________________

  # Extract CH from all country polygons and create buffer polygon

  chdf_buffer<-get_chdf_buffer(gdf, params$SWISS_BUFFER)$chdf_buffer

  # Transform country polygons to projection 3035 in order to get it in meter-units
  chdf_3035<-get_chdf_buffer(gdf, params$SWISS_BUFFER)$chdf_3035

  # The change in sf v1.0 was move from backend engine for
  # unprojected coordinates (the geographic ones, i.e. lat - long as in EPSG 4326) from GEOS to s2 from Google
  suppressMessages({
    sf::sf_use_s2(FALSE)
  })

  # Geocode spatial points from raw data without internet points
  # These transactions are not on-site but online transactions.
  # Due to Adobe headquarters in Ireland, however, online transactions may be misinterpreted as travel.
  # Therefore, these transactions need to be excluded from mobility features
  internetpoints <- create_internetpoints(df_raw, params$LAT_COLUMN, params$LON_COLUMN)
  sf_cardpoints <- get_sf_cardpoints(df_sp, gdf, internetpoints, chdf_buffer, chdf_3035, params$SWISS_BUFFER)
  sf_cardpoints <- add_foreign_countries(sf_cardpoints)
  features <- add_mobility_results(features, sf_cardpoints)

  rm(sf_cardpoints, internetpoints, gdf, chdf_buffer, chdf_3035)
  gc(verbose = FALSE)
  futile.logger::flog.info("Added mobility features")
  setTxtProgressBar(pb, 6)
  cat("\n")

  # ______________________________________________________________________

  # 7. Output ####
  # ______________________________________________________________________

    # Fill missing values with sensible defaults
  features <- fill_default_values(features)
  
  # DEBUG: Write results to csv
  if (dev_mode) {
    write.csv2(features, params$OUT_PATH)
    futile.logger::flog.info("Exported features to %s", params$OUT_PATH)
  }

  # Validate results before writing to data base
  if (!validate_results(features, df, params$CUSTOMER_ID_COLUMN, params$MISSING_THRESHOLD_PCT)) {
    if (!dev_mode) {
      return(FALSE)
    }
  }

  # Export features to target data base table
  tryCatch({

    if(dev_mode){

      DBI::dbWriteTable(
        conn = odbc_conn,
        "out_data",
        features,
        overwrite = T, row.names = F, fileEncoding = "latin1"
      )
      futile.logger::flog.info(
        "Wrote features to data base table %s", "out_data")
    }

    else{

      DBI::dbWriteTable(
        conn = odbc_conn,
        DBI::Id(schema = 'dbo', table = params$OUT_DB),
        features,
        overwrite = T, row.names = F, fileEncoding = "latin1"
      )
      futile.logger::flog.info(
        "Wrote features to data base table %s", params$OUT_DB)

 }

    setTxtProgressBar(pb, 7)
    cat("\n")
    return(TRUE)
  }, error = function(e) {
    futile.logger::flog.error("Failed to write results to data base: %s", e)

  })

  return(FALSE)
}
