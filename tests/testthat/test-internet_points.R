
test_that("create_internetpoints returns spatial data frame", {
  out <- create_internetpoints(df, params$LAT_COLUMN, params$LON_COLUMN)
  expect_true(all(c("sf", "data.frame") %in% class(out)))
})
