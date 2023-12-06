test_that("Check create_sets_rndst works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  expect_error(create_sets_rndst(obs), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_rndst(obs), "obs is empty")

  # -- 3rd example: sounds good, check test and train dimensions, class
  obs <- fread(paste0(
    "../input/",
    "NC-monitors-dailysummary-",
    "20220601-20220831",
    "-space-time-covariates.csv"
  ))
  p <- c(as.Date("2022-08-01"), as.Date("2022-08-02"))
  obs <- obs[date %in% p, ]
  sets <- create_sets_rndst(obs)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
})


test_that("Check create_sets_rnds works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  expect_error(create_sets_rnds(obs), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_rnds(obs), "obs is empty")

  # -- 3rd example: sounds good, check test and train dimensions, class
  obs <- fread(paste0(
    "../input/",
    "NC-monitors-dailysummary-",
    "20220601-20220831",
    "-space-time-covariates.csv"
  ))
  p <- c(as.Date("2022-08-01"), as.Date("2022-08-02"))
  obs <- obs[date %in% p, ]
  sets <- create_sets_rnds(obs)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
  expect_equal(
    sum(unique(train$station) %in% unique(test$station)),
    0
  ) # means all stations are different in each sets
})


test_that("Check create_sets_t works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  test_dates <- as.Date("2022-08-01")
  expect_error(create_sets_t(obs, test_dates), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_t(obs, test_dates), "obs is empty")

  # -- 3rd example: test_dates is not a "Date"
  test_dates <- "hello"
  obs <- fread(paste0(
    "../input/",
    "NC-monitors-dailysummary-",
    "20220601-20220831",
    "-space-time-covariates.csv"
  ))
  expect_error(create_sets_t(obs, test_dates), "test_dates is not a Date")

  # -- 4th example: some of test_dates are not in obs
  test_dates <- c(as.Date("2022-08-01"), as.Date("2100-01-01"))
  expect_error(
    create_sets_t(obs, test_dates),
    "some of test_dates are not in obs"
  )

  # -- 5th example: training set is empty
  test_dates <- c(as.Date("2022-08-01"), as.Date("2022-08-02"))
  expect_warning(
    create_sets_t(obs[date %in% test_dates, ], test_dates),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  sets <- create_sets_t(obs, test_dates)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
  expect_equal(
    sum(unique(train$date) %in% unique(test$date)),
    0
  ) # means all dates are different in each sets
})


test_that("Check create_sets_s works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  test_counties <- "Durham"
  expect_error(create_sets_s(obs, test_counties), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_s(obs, test_counties), "obs is empty")

  # -- 3rd example: test_counties is not a "character"
  test_counties <- 3
  obs <- fread(paste0(
    "../input/",
    "NC-monitors-dailysummary-",
    "20220601-20220831",
    "-space-time-covariates.csv"
  ))
  obs <- add_nc_county(obs, "epsg:4326")
  expect_error(
    create_sets_s(obs, test_counties),
    "test_counties is not a character or is empty"
  )

  # -- 4th example: some of test_counties are not in obs
  test_counties <- c("Wake", "Aveyron")
  expect_error(
    create_sets_s(obs, test_counties),
    "some of test_counties are not in obs"
  )

  # -- 5th example: training set is empty
  test_counties <- c("Durham", "Orange")
  expect_warning(
    create_sets_s(obs[county %in% test_counties, ], test_counties),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  sets <- create_sets_s(obs, test_counties)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
  expect_equal(
    sum(unique(train$county) %in% unique(test$county)),
    0
  ) # means all counties are different in each sets
})



test_that("Check create_sets_net works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  test_net <- "ECONET"
  expect_error(create_sets_net(obs, test_net), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_net(obs, test_net), "obs is empty")

  # -- 3rd example: test_net is not a "character"
  test_net <- 3
  obs <- fread(paste0(
    "../input/",
    "NC-monitors-dailysummary-",
    "20220601-20220831",
    "-space-time-covariates.csv"
  ))
  expect_error(
    create_sets_net(obs, test_net),
    "test_net is not a character or is empty"
  )

  # -- 4th example: some of test_net are not in obs
  test_net <- c("netatmo")
  expect_error(
    create_sets_net(obs, test_net),
    "some of test_net are not in obs"
  )

  # -- 5th example: training set is empty
  test_net <- c("ECONET")
  expect_warning(
    create_sets_net(obs[network %in% test_net, ], test_net),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  sets <- create_sets_net(obs, test_net)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
  expect_equal(
    sum(unique(train$network) %in% unique(test$network)),
    0
  ) # means all counties are different in each sets
})
