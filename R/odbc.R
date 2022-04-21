#' Connect to ODBC data base
#'
#' @param dev_mode Boolean. If TRUE, connect to a local test DB.
#' @return A connection object to be used by DBI::dbConnect().
#'
odbc_connect <- function(DATA_PATH_DB = NULL, dev_mode = F, connection_config = NULL) {

  if (dev_mode && is.null(DATA_PATH_DB)) {
    futile.logger::flog.error("Required argument 'DATA_PATH_DB' not provided to odbc_connect running in dev_mode")
    return(NULL)
  }

  if (dev_mode) {
    # Create an SQLIte data base on the fly and fill it with test data from an Excel spreadsheet.
    db_name <- paste0(
      "tests/test_db_",
      format(Sys.time(), format = "%Y%m%d%H%M%S"),
      ".db"
    )
    futile.logger::flog.debug("Dev mode ON => Connecting to test data base %s", db_name)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
    test_dat <- readxl::read_excel("tests/20211020_test_data_ptc_fakeData.xlsx")
    DBI::dbWriteTable(con, DATA_PATH_DB, test_dat)
  }
  else {
    con <- NULL
    tryCatch({
      suppressWarnings({
        if (is.null(connection_config)) {
          futile.logger::flog.error("odbc_connect: Missing connection_config")
          return(NULL)
        } else if (is.character(connection_config)) {
          # If a file path was passed to the function, load the yaml file
          odbc_data <- yaml::read_yaml(base::file(connection_config, 'r'))
        } else {
          # If a pre-loaded configuration was passed to the function, use that.
          # This is used for unit testing.
          odbc_data <- connection_config
        }
        db_selected <- odbc_data$kbda_lab
        con <-  DBI::dbConnect(
          odbc(),
          Driver = db_selected$driver,
          Server = db_selected$server,
          Database = db_selected$database,
          Trusted_Connection = db_selected$trusted_connection,
          encoding = 'latin1'
        )
      })
    }, error = function(e) {
      futile.logger::flog.error("Failed to connect to ODBC data base: %s", e)
    })
  }

  return(con)
}
