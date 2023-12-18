test_that("Check open_pred_period works", {
  data_path <- "../testdata/"
  # -- 1st example: period is not a Date object
  p <- c("2022-06-01", "2022-06-03")
  expect_error(open_pred_period(period = p, data_path = data_path),
               "period is not a Date")

  # -- 2nd example: date is not in files
  p <- c(as.Date("2022-08-01"), as.Date("2100-06-03"))
  expect_error(open_pred_period(period = p, data_path = data_path),
               "date 2100-06-03 is not in files")

  # -- 3rd example: should work
  p <- c(as.Date("2022-08-01"), as.Date("2022-08-02"))
  expect_no_error(open_pred_period(period = p, data_path = data_path))
  output <- open_pred_period(period = p, data_path = data_path)
  expect_equal(class(output), "list")
  expect_equal(names(output), as.character(p))
})
