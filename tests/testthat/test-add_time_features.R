test_that("evaluate_tomorrow_freeday works", {
  free <- as.POSIXct("2021-10-16", "2021-10-15")
  not_free <- as.POSIXct("2021-10-17", "2021-10-18")
  expect_true(all(evaluate_tomorrow_freeday(free)))
  expect_false(any(evaluate_tomorrow_freeday(not_free)))
})

evaluate_today_freeday
test_that("evaluate_today_freeday works", {
  free <- as.POSIXct("2021-10-16", "2021-10-17")
  not_free <- as.POSIXct("2021-10-15", "2021-10-18")
  expect_true(all(evaluate_today_freeday(free)))
  expect_false(any(evaluate_today_freeday(not_free)))
})
