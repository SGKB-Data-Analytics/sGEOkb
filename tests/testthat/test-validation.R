
################################################################################
# Test validate_parameters
################################################################################

test_that("validate_parameters fails if a required variable is not defined in the parameters", {
  dat <- lapply(list_required_variables(), function(x) "a")
  names(dat) <- list_required_variables()
  dat$CUSTOMER_ID_COLUMN <- NULL
  expect_false(validate_parameters(dat))
})

test_that("validate_parameters succeeds if only required variables are defined in the parameters", {
  dat <- lapply(list_required_variables(), function(x) "a")
  names(dat) <- list_required_variables()
  expect_true(validate_parameters(dat))
})

test_that("validate_parameters succeeds if required and further variables are defined in the parameters", {
  dat <- lapply(list_required_variables(), function(x) "a")
  names(dat) <- list_required_variables()
  dat$FURTHER_VARIABLE <- "b"
  expect_true(validate_parameters(dat))
})


################################################################################
# Test validate_connection_config
################################################################################

test_that("validate_connection_config succeeds if all required elements are present", {
  expect_true(validate_connection_config("connection_config_valid_test.yml"))
})

test_that("validate_connection_config fails if connection config is not a yaml file", {
  expect_false(validate_connection_config("not_a_yaml_file.csv"))
  expect_false(validate_connection_config("not_a_yaml_file.R"))
  expect_false(validate_connection_config("not_a_yaml_file.json"))
})

connection_config <- yaml::read_yaml("connection_config_valid_test.yml")

test_that("validate_connection_config fails if kbda_lab is missing", {
  # connection_config$placeholder <- 1
  connection_config$kbda_lab <- NULL
  expect_false(validate_connection_config(connection_config))
})

test_that("validate_connection_config fails if any required element is missing", {
  connection_config$kbda_lab$driver <- NULL
  expect_false(validate_connection_config(connection_config))
})

test_that("validate_connection_config fails if any required element is NA or NULL", {
  connection_config$kbda_lab$driver <- NA
  expect_false(validate_connection_config(connection_config))
})


################################################################################
# Test validate_countries_data
################################################################################

countries_valid <- geojsonsf::geojson_sf("countries_test.geojson")

test_that("validate_countries_data accepts valid object", {
  expect_true(validate_countries_data(countries_valid))
})

test_that("validate_countries_data rejects non-sf objects", {
  countries_invalid <- countries_valid
  class(countries_invalid) <- c("data.frame")
  expect_false(validate_countries_data(countries_invalid))
})

test_that("validate_countries_data fails when required column missing", {
  countries_invalid <- countries_valid
  countries_invalid$ISO_A3 <- NULL
  expect_false(validate_countries_data(countries_invalid))
})

test_that("validate_countries_data fails when geometry for Switzerland invalid", {
  countries_invalid <- countries_valid
  # The geometry of Antarctica (ADMIN = "ATA") is invalid.
  countries_invalid[countries_invalid$ISO_A3 == "CHE", "geometry"] <-
    countries_invalid[countries_invalid$ISO_A3 == "ATA", "geometry"]
  expect_false(validate_countries_data(countries_invalid))
})

rm(countries_valid)

################################################################################
# Test validate_mapping_files
################################################################################

mapping_files <- list(
  # This mapping contains duplicates, i.e. certain values have more than one key
  GoogT_Mapping_json = jsonlite::read_json(
    "googletypes_synonyms.json",
    simplifyVector = T
  )
)

test_that("validate_mapping_files rejects when value occurs for more than one key", {
  expect_false(validate_mapping_files(mapping_files))
})

test_that("validate_mapping_files rejects empty mapping files", {
  mapping_files$empty_list <- list()
  mapping_files$missing_element <- NA
  expect_false(validate_mapping_files(mapping_files))
})

rm(mapping_files)


################################################################################
# Test validate_transaction_data
################################################################################

test_that("validate_transaction_data accepts valid test data", {
  expect_true(validate_transaction_data(df, params))
})

test_that("validate_transaction_data fails if no data or parameters are provided", {
  expect_false(validate_transaction_data(transaction_data = df))
  expect_false(validate_transaction_data(params = params))
})

test_that("validate_transaction_data rejects non-dataframe", {
  expect_false(validate_transaction_data(list(df), NULL))
  class(df) <- NULL
  expect_false(validate_transaction_data(df, NULL))
})

test_that("validate_transaction_data rejects empty dataframe", {
  expect_false(validate_transaction_data(data.frame(), params))
})

test_that("validate_transaction_data fails if a required column is missing", {
  df$google_types <- NULL
  expect_false(validate_transaction_data(df, params))
})

test_that("validate_transaction_data succeeds if more than required columns are present", {
  df$additional_column <- 1
  expect_true(validate_transaction_data(df, params))
})

################################################################################
# Test validate_raster_files
################################################################################

test_that("validate_raster_files fails if RASTERS is empty", {
  expect_false(validate_raster_files("inexistent_folder", c()))
  expect_false(validate_raster_files("empty_raster_stack", c()))
  expect_false(validate_raster_files(params$RASTERSTACK_PATH, c()))
})

test_that("validate_raster_files succeeds if rasters are valid", {
  expect_true(validate_raster_files(params$RASTERSTACK_PATH, params$RASTERS))
})

test_that("validate_raster_files fails if some raster file is not in RASTERSTACK_PATH", {
  params$RASTERS <- c(params$RASTERS, "inexistent_file")
  expect_false(validate_raster_files(params$RASTERSTACK_PATH, params$RASTERS))
})

test_that("validate_raster_files fails if some raster file is not in tif-format", {
  params$RASTERS <- c("not_a_tif.txt")
  params$RASTERSTACK_PATH <- "empty_raster_stack"
  test_file <- file.path(params$RASTERSTACK_PATH, params$RASTERS)
  if (!file.exists(test_file)) file.create(test_file)
  expect_false(validate_raster_files(params$RASTERSTACK_PATH, params$RASTERS))
})

# test_that("validate_raster_files fails if rasters' resolutions do not match", {
#   # TODO
# })
#
# test_that("validate_raster_files fails if rasters' extent does not match", {
#   # TODO
# })


################################################################################
# Test validate_results
################################################################################

# The following test data have several columns that exceed the missing value pct threshold.
invalid_features <- read.table(
  "invalid_features_test.csv",
  sep = ";",
  header = T,
  stringsAsFactors = F
)

# Create valid features by removing offending columns from invalid_features
pct_missing_per_var <- apply(invalid_features, 2, function(col) {
  100*sum(is.na(col)) / length(col)
})
too_many_missings <- pct_missing_per_var > params$MISSING_THRESHOLD_PCT
valid_features <- invalid_features[, !too_many_missings]

test_that("validate_results succeeds for a valid test data set", {
  expect_true(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if features is not a data frame", {
  class(valid_features) <- "list"
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if features is an empty data frame", {
  expect_false(validate_results(
    data.frame(),
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if features has no 'id' column", {
  valid_features$id <- NULL
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if there are duplicate values in features' 'id' column", {
  valid_features <- rbind(valid_features, valid_features)
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if features does not contain all customer IDs from the transaction data", {
  valid_features <- valid_features[-1, ]
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if there are any features containing only missing values", {
  valid_features$mob_country_most_visited_pct <- NA
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if there are any records (= customers) containing only missing values", {
  valid_features[1, ] <- NA
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if any features exceed the allowable percentage of missing values", {
  expect_false(validate_results(
    invalid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

test_that("validate_results fails if any features have zero variance", {
  valid_features$mob_country_most_visited_pct <- 1
  expect_false(validate_results(
    valid_features,
    df,
    params$CUSTOMER_ID_COLUMN,
    params$MISSING_THRESHOLD_PCT
  ))
})

rm(invalid_features)
