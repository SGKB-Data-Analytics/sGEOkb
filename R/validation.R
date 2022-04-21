# Validation functions assert that a data input corresponds to certain criteria.
# These functions return TRUE or FALSE. Should they return FALSE,
# then they also log the cause of the rejection(s) at the log level ERROR.
# The upstream functions calling validation functions should not print
# any error messages to avoid verbosity / redundancy.


#' Check that required parameters are present
#'
#' @param params A named list containing parameters as key-value pairs.
#'
#' @return A boolean indicating if the validation was successful.
#'
validate_parameters <- function(params) {

  valid <- TRUE
  required_params <- list_required_variables()

  for (var in required_params) {
    if (!var %in% names(params)) {
      futile.logger::flog.error("Parameter %s not provided in parameters file", var)
      valid <- FALSE
    }
  }

  return(valid)
}


#' Validate connection configuration file
#'
#' Yaml file should have one element 'kbda_lab' with several children.
#' @param connection_config The path to the config file.
#'
#' @return A boolean.
#'
validate_connection_config <- function(connection_config) {

  if (is.character(connection_config)) {
    # Assert that connection_config is a yaml file
    if (!endsWith(connection_config, ".yml") & !endsWith(connection_config, ".yaml")) {
      futile.logger::flog.error("Connection config file expected in yaml format")
      return(FALSE)
    }
    odbc_data <- yaml::read_yaml(base::file(connection_config, 'r'))
  } else {
    odbc_data <- connection_config
  }

  # Require element kbda_lab
  if (!"kbda_lab" %in% names(odbc_data)) {
    futile.logger::flog.error("Connection config file %s is invalid: Missing element 'kbda_lab'")
    return(FALSE)
  }
  kbda_lab <- odbc_data$kbda_lab

  # Assert that all required variables are defined
  required_variables <- c("driver", "server", "database", "trusted_connection")
  missing_variables <- !required_variables %in% names(kbda_lab)
  if (any(missing_variables)) {
    futile.logger::flog.error(
      "Connection config file is missing the following elements: '%s'",
      paste(required_variables[missing_variables], collapse = "', '")
    )
    return(FALSE)
  }

  # Assert that no required variables have null or missing values
  missing_or_null_values <- sapply(required_variables, function(var) {
    is.na(kbda_lab[[var]]) || is.null(kbda_lab[[var]])
  })
  if (any(missing_or_null_values)) {
    futile.logger::flog.error(
      "The following elements in connection config are missing or NULL: '%s'",
      paste(required_variables[missing_or_null_values], collapse = "', '")
    )
    return(FALSE)
  }

  return(TRUE)
}


#' Validate countries data
#'
#' @param countries A data frame of class sf.
#' @return A boolean indicating if countries is valid.
#'
validate_countries_data <- function(countries) {

  valid <- TRUE

  # Is countries an sf object
  if (!"sf" %in% class(countries)) {
    futile.logger::flog.error("Countries object is not of class 'sf'")
    valid <- FALSE
  }

  # Does countries contain the required columns
  required_vars <- c("ISO_A3", "geometry")
  if (!all(required_vars %in% colnames(countries))) {
    missing_vars <- required_vars[!required_vars %in% colnames(countries)]
    futile.logger::flog.error(
      "Countries data frame is missing the required columns '%s'",
      paste(missing_vars, collapse = "', '")
    )
    valid <- FALSE
  }

  # Is countries' geometry for Switzerland is valid. Skip this check if valid == FALSE already.
  switzerland <- countries[countries$ISO_A3 == "CHE", ]
  if (valid && !suppressWarnings(sf::st_is_valid(switzerland))) {
    futile.logger::flog.error(
      "The geometry for Switzerland is invalid. Reason: %s",
      sf::st_is_valid(switzerland, reason = T)
    )
    valid <- FALSE
  }

  return(valid)
}


#' Validate mapping files
#'
#' @param mapping_files A list of mapping files.
#' @return A boolean.
#'
validate_mapping_files <- function(mapping_files) {

  valid <- TRUE

  # Mapping files are lists that represent mappings key => list(values).
  # The key represents a businesstype, and the values are key words associated with that businesstype.
  # When businesstype is missing from the transaction data, then it is looked up if
  # any of the values occur in the relevant columns (e.g. BOOKING_COLUMN, GOOLGE_COLUMN).
  # If so, then the key is assigned to the businesstype.
  # For this to work properly, each value can only be associated with ONE keys.

  # Assert that all mapping files are non-empty lists.
  # Empty mapping files indicate corrupted file paths.
  # Do not proceed with furhter checks if this check fails.
  index <- sapply(mapping_files, function(mapping) {
    is.list(mapping) && length(mapping) > 0
  })
  if (!all(index)) {
    futile.logger::flog.error(
      "The following mapping files are not lists or empty: '%s'",
      paste(mapping_files[!index], collapse = "', '")
    )
    return(FALSE)
  }

  # Assert that each value is only associated with ONE key.
  for (mapping in mapping_files) {
    values <- unlist(do.call(c, mapping))
    duplicate_index <- duplicated(values)
    if (sum(duplicate_index) > 0) {
      futile.logger::flog.error(
        "In mapping files: The following values are duplicated: '%s'",
        paste(values[duplicate_index], collapse = "', '")
      )
      valid <- FALSE
    }
  }

  return(valid)
}


#' Validate transaction data
#'
#' @param transaction_data A data frame.
#' @param params A named list of parameters such as returned by load_parameters().
#' @return A boolean.
#'
validate_transaction_data <- function(transaction_data = NULL, params = NULL) {

  # Assert that all arguments are provided
  if (is.null(transaction_data)) {
    futile.logger::flog.error("Did not provide transation data")
    return(FALSE)
  }
  if (is.null(params)) {
    futile.logger::flog.error("Did not provide params")
    return(FALSE)
  }

  # Assert that data are a data frame
  if (!is.data.frame(transaction_data)) {
    futile.logger::flog.error("The provided transaction data are not a data frame.")
    return(FALSE)
  }

  # Assert that the data are non-empty
  if (nrow(transaction_data) == 0) {
    futile.logger::flog.error("The provided transaction data have 0 rows")
    return(FALSE)
  }

  # Assert that the required columns are present
  required_columns <- unlist(params[endsWith(names(params), "_COLUMN")])
  missing_index <- !required_columns %in% colnames(transaction_data)
  if (sum(missing_index) > 0) {
    futile.logger::flog.error(
      "The following columns are missing from the transaction data: '%s'",
      paste(required_columns[missing_index], collapse = "', '")
    )
    return(FALSE)
  }

  return(TRUE)
}


#' Validate raster files
#'
#' @param RASTERSTACK_PATH String. The path to the directory containing the raster files.
#' @param RASTERS Character vector containing the names of the raster files.
#' @return A boolean.
#'
validate_raster_files <- function(RASTERSTACK_PATH, RASTERS) {

   # Check that RASTERSTACK_PATH exists
  if (!file.exists(RASTERSTACK_PATH)) {
    futile.logger::flog.error(paste0("Path to raster files does not exist (",getwd(),"/", params$RASTERSTACK_PATH, ")"))
    return(FALSE)
  }


  # Assert that the list of raster files is non-empty.
  if (is.null(RASTERS) || is.na(RASTERS) || length(RASTERS) == 0) {
    futile.logger::flog.error("No raster files are specified")
    return(FALSE)
  }

  # Determine raster files from parameters.
  raster_files <- list.files(path = RASTERSTACK_PATH , pattern = '*.tif$', full.names = TRUE)
  raster_files <- raster_files[grepl(x = raster_files, pattern = paste(RASTERS, collapse = "|"))]

  # Check that all files specified in RASTERS are available in the tif-format
  raster_not_found <- !RASTERS %in% sub("\\.tif", "", basename(raster_files))
  if (sum(raster_not_found) > 0) {
    futile.logger::flog.error(
      "The following raster files are not available in the tif-format: '%s'",
      paste(RASTERS[raster_not_found], collapse = "', '")
    )
    return(FALSE)
  }

  ###############################################
  # Assert that all raster files are of the same resolution and extent
  # (required by raster::stack() function)
  ###############################################
  rasters <- lapply(raster_files, raster::raster)
  resolutions <- lapply(rasters, raster::res)
  extents <- lapply(rasters, raster::extent)

  # Resolution can be vector of length one or two. Should be the same for all rasters
  lengths_resolutions <- sapply(resolutions, length)
  if (!all(lengths_resolutions == lengths_resolutions[1])) {
    futile.logger::flog.error("The length of the raster files' resolutions varies")
    return(FALSE)
  }
  resolutionsMatrix <- do.call(cbind, resolutions)
  resMatRowsEqual <- apply(resolutionsMatrix, 1, function(row) all(row == row[1]))
  if (!all(resMatRowsEqual)) {
    futile.logger::flog.error("The raster files' resolutions are not the same")
    return(FALSE)
  }

  # Extent objects have four slots: xmin, xmax, ymin, ymax. Should be the same for all rasters
  extentsList <- lapply(extents, as.list)
  extentsMatrix <- do.call(cbind, extentsList)
  extMatRowsEqual <- apply(extentsMatrix, 1, function(row) all(row == row[1]))
  if (!all(extMatRowsEqual)) {
    futile.logger::flog.error("The raster files' extents are not the same")
    return(FALSE)
  }

  return(TRUE)
}


#' Validate result data
#'
#' @param features The data frame containing the result data
#' @return A boolean
#'
validate_results <- function(features, df, CUSTOMER_ID_COLUMN, MISSING_THRESHOLD_PCT) {

  valid <- TRUE

  # Check that features is a non-empty data frame with an ID-column
  # that contains only unique id-values
  if (!is.data.frame(features)) {
    futile.logger::flog.error("Results object is not a data frame.")
    return(FALSE)
  }

  if (nrow(features) == 0) {
    futile.logger::flog.error("Results are an empty data frame.")
    return(FALSE)
  }

  if (!"id" %in% colnames(features)) {
    futile.logger::flog.error("Results data frame does not contain a customer ID column 'id'")
    return(FALSE)
  }

  if (anyDuplicated(features$id)) {
    futile.logger::flog.error("Results data frame contains duplicate customer IDs in column 'id'")
    valid <- FALSE
  }

  # Assert that every customer ID from transactions data is in the features data frame
  ids_original <- unique(df[, CUSTOMER_ID_COLUMN])
  ids_results <- unique(features$id)
  missing_ids <- !ids_original %in% ids_results
  if (any(missing_ids)) {
    futile.logger::flog.error(
      "The following customer IDs are missing from results: '%s'",
      paste(ids_original[missing_ids], collapse = "', '")
    )
    valid <- FALSE
  }

  ### Missing values

  # Assert that for no row and no column all values are NA
  rowsAllNA <- apply(features, 1, function(row) all(is.na(row)))
  colsAllNA <- apply(features, 2, function(col) all(is.na(col)))
  if (any(rowsAllNA)) {
    futile.logger::flog.error(
      "The records for the following customer IDs contain only missing values: '%s'",
      paste(features[rowsAllNA, "id"], collapse = "', '")
    )
    valid <- FALSE
  }
  if (any(colsAllNA)) {
    futile.logger::flog.error(
      "The following features contain only missing values: '%s'",
      paste(colnames(features)[colsAllNA], collapse = "', '")
    )
    valid <- FALSE
  }

  # Assert that no one variable contains more than MISSING_THRESHOLD_PCT percent
  # missing values. MISSING_THRESHOLD_PCT is configured in the parameters file.
  pct_missing_per_var <- apply(features, 2, function(col) {
    100*sum(is.na(col)) / length(col)
  })
  too_many_missings <- pct_missing_per_var > MISSING_THRESHOLD_PCT
  if (any(too_many_missings)) {
    futile.logger::flog.error(
      "The following features of the result data contain too many missing variables: '%s'",
      paste(colnames(features)[too_many_missings], collapse = "', '")
    )
    valid <- FALSE
  }

  # Assert that for no numeric column the variance is zero.
  # Exception: Column "total_activity" measures the number of transactions per customer.
  # In test data this number may be constant.
  excluded_features <- colnames(features) %in% c("total_activity")
  numeric_features <- sapply(features, class) %in% c("numeric", "integer")
  varPerFeature <- sapply(features, function(col) {
    if (class(col) %in% c("numeric", "integer")) return(var(col, na.rm = T))
    else return(NA)
  })
  varianceIsZero <- is.na(varPerFeature) | varPerFeature == 0
  varianceIsZero <- numeric_features & varianceIsZero & !excluded_features
  if (any(varianceIsZero)) {
    futile.logger::flog.error(
      "The following features have zero variance: '%s'",
      colnames(features)[varianceIsZero]
    )
    valid <- FALSE
  }

  return(valid)
}
