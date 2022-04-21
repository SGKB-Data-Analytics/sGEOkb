library(jsonlite)


################################################################################
# Test load_parameters
################################################################################

test_that("load_parameters returns NULL if file not found", {
  file <- "inexistentFile"
  if (file.exists(file)) {
    file.remove(file)
  }
  expect_null(load_parameters(file))
})

test_that("load_parameters returns NULL if file not in yaml format", {
  file <- "thisWillFail.json"
  if (file.exists(file)) {
    file.remove(file)
  }
  dat <- jsonlite::toJSON(iris, pretty = T)
  write(dat, file = file)
  expect_null(load_parameters(file))
  file.remove(file)
})

test_that("load_parameters does not return NULL for an existing yaml-file", {
  file <- "validFile"
  if (file.exists(file)) {
    file.remove(file)
  }
  dat <- yaml::write_yaml(iris, file = file)
  expect_null(load_parameters(file))
  file.remove(file)
})

