#' Load Parameters from Parameter File
#'
#' @param file String. The path to the yaml-document containing the parameter definitions.
#' @return A named list containing the parameters as key-value pairs.
#' @export
#'
load_parameters <- function(file = NULL) {

  if (is.null(file)) {
    futile.logger::flog.error("No filename provided to load_parameters")
    return(NULL)
  }

  parameters <- NULL

  if (!endsWith(file, "yaml") && !endsWith(file, "yml")) {
    futile.logger::flog.error("Provided file is not in yaml format")
    return(NULL)
  }

  tryCatch({
    parameters <- yaml::read_yaml(file)
  }, error = function(e) {
    futile.logger::flog.error("Failed to read parameters from file %s: %s", file, e)
  })

  return(parameters)
}


#' List all required variables of a parameters file
#'
#' @return A character vector containing the variable names.
#' @export
#'
list_required_variables <- function() {
  required_params <- c(
    "CUSTOMER_ID_COLUMN",
    "TIME_COLUMN",
    "AMOUNT_COLUMN",
    "LOCATION_COLUMN",
    "ALT_LOCATION_COLUMN",
    "BOOKING_COLUMN",
    "GOOGLE_COLUMN",
    "LAT_COLUMN",
    "LON_COLUMN",
    "SWISS_BUFFER",
    "DATA_PATH_DB",
    # "INPUTS_PATH",   # Not required. If provided, it overrides system.file(package = "sGEOkb")
    "RASTERSTACK_PATH",
    "OUT_PATH",
    "OUT_DB",
    "MISSING_THRESHOLD_PCT",
    "RASTERS"
  )
}
