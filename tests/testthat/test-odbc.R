
dir.create("tests/testthat", recursive = T)
file.copy("../test_data_ptc_TRX_MIT_KUNDE_20210525.xlsx", "tests/")
this_con <- odbc_connect(params$DATA_PATH_DB, dev_mode = T)

test_that("odbc_connect in dev_mode returns valid object", {
  expect_equal(params$DATA_PATH_DB, "test_data")
  expect_true("SQLiteConnection" == class(this_con))
  expect_equal(length(DBI::dbListTables(this_con)), 1)
  expect_true(params$DATA_PATH_DB %in% DBI::dbListTables(this_con))
})

unlink("tests/testthat", recursive = T)
unlink("tests", recursive = T)
