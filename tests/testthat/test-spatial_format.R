  test_that("transform_to_spatial_format returns spatial data frame", {
    df_sp <- transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    )
    expect_true(all(c("data.frame", "sf") %in% class(df_sp)))
  })

  test_that("transform_to_spatial_format fails if LAT_COLUMN is not in df", {
    expect_null(transform_to_spatial_format(
      df,
      "inexistent_column",
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))
    df[[params$LAT_COLUMN]] <- NULL
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))

  })

  test_that("transform_to_spatial_format fails if LON_COLUMN is not in df", {
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      "inexistent_column",
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))
    df[[params$LON_COLUMN]] <- NULL
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))

  })

  test_that("transform_to_spatial_format fails if TIME_COLUMN is not in df", {
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      "inexistent_column",
      params$CUSTOMER_ID_COLUMN
    ))
    df[[params$TIME_COLUMN]] <- NULL
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))

  })

  test_that("transform_to_spatial_format fails if CUSTOMER_ID_COLUMN is not in df", {
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      "inexistent_column"
    ))
    df[[params$CUSTOMER_ID_COLUMN]] <- NULL
    expect_null(transform_to_spatial_format(
      df,
      params$LAT_COLUMN,
      params$LON_COLUMN,
      params$TIME_COLUMN,
      params$CUSTOMER_ID_COLUMN
    ))
  })

rm(params, df, con)
