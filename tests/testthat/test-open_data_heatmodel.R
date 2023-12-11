test_that("Check open_pred_period works", {
  skip()
  # -- 1st example: period is not a Date object
  p <- c("2022-06-01", "2022-06-03")
  expect_error(open_pred_period(p), "period is not a Date")

  # -- 2nd example: date is not in files
  p <- c(as.Date("2022-06-01"), as.Date("2100-06-03"))
  expect_error(open_pred_period(p), "date 2100-06-03 is not in files")

  # -- 3rd example: return a data.table with all covariates
  p <- c(as.Date("2022-08-01"))
  output <- open_pred_period(p)
  expect_equal(output, c("data.table", "data.frame"))
  expect_contains(
    colnames(output),
    c(
      "lon", "lat", "date", "geo",
      "tcc", "build.fp", "dem", "build.h",
      "TNwmo", "TN7am", "TN12am", "TXwmo", "TX7am", "TX12am"
    )
  )
})
