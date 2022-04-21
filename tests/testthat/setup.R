
# Assert that the test data referenced in unit tests is there
required_files <- c(
  "parameters_test.yaml",
  "db_test.db",
  "countries_test.geojson",
  "googletypes_synonyms.json",
  "invalid_features_test.csv"
)
if (!all(file.exists(required_files))) {
	flog.error(
		"Missing the following files in the testthat folder: '%s'",
		paste(required_files[!file.exists(required_files)], collapse = "', '")
	)
	stop()
}

# Switch off logs from functions during automated testing
flog.appender(function(line) {})

# Create some data used by all tests
params <- load_parameters("parameters_test.yaml")
con <- DBI::dbConnect(RSQLite::SQLite(), "db_test.db")
df <- DBI::dbGetQuery(conn = con, statement = paste0("Select * from test_data"))
df_sp <- transform_to_spatial_format(
  df,
  params$LAT_COLUMN,
  params$LON_COLUMN,
  params$TIME_COLUMN,
  params$CUSTOMER_ID_COLUMN
)
