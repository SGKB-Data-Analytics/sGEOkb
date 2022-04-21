
test_that("get_rasterstack_features returns a spatial data frame with correct dimensions", {
  number_of_raster_layers <- 2
  out <- get_rasterstack_features(df_sp, params$RASTERSTACK_PATH, params$RASTERS[1:number_of_raster_layers])
  expect_true(all(c("sf", "data.frame") %in% class(out)))
  expect_equal(nrow(df_sp), nrow(out))
  expect_equal(ncol(df_sp) + number_of_raster_layers, ncol(out))
})
