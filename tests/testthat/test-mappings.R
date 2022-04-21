
test_that(".default_if_file_not_exists returns original path if file exists", {
  out <- .default_if_file_not_exists(getwd(), "countries_test.geojson")
  expect_equal(out, file.path(getwd(), "countries_test.geojson"))
})

test_that(".default_if_file_not_exists returns default path if file does not exist", {
  out <- .default_if_file_not_exists(getwd(), "countries.geojson")
  expect_equal(out, file.path(system.file(package = "sGEOkb"), "countries.geojson"))
})

test_that(".default_if_file_not_exists fails if default file does not exist", {
  out <- .default_if_file_not_exists(getwd(), "inexistent.geojson")
  expect_null(out)
})
