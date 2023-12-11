test_that("Check create_sets_rndst works", {
  # -- 1st example: obs is not a data.table
  obs <- "hello"
  expect_error(create_sets_rndst(obs), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_rndst(obs), "obs is empty")

  # -- 3rd example: sounds good, check test and train dimensions, class
  obs <- fread("../testdata/rtp_points.csv")
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
  obs <- fread("../testdata/rtp_points.csv")
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
  test_times <- as.Date("2022-08-01")
  expect_error(create_sets_t(obs, test_times), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_t(obs, test_times), "obs is empty")

  # -- 3rd example: test_times is not a "Date"
  test_times <- "hello"
  obs <- fread("../testdata/rtp_points.csv")
  expect_error(create_sets_t(obs, test_times), "test_times is not a Date")

  # -- 4th example: some of test_times are not in obs
  test_times <- c(as.Date("2022-08-01"), as.Date("2100-01-01"))
  expect_error(
    create_sets_t(obs, test_times),
    "some of test_times are not in obs"
  )

  # -- 5th example: training set is empty
  test_times <- c(as.Date("2022-08-01"), as.Date("2022-08-02"))
  expect_warning(
    create_sets_t(obs[time %in% test_times, ], test_times),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  sets <- create_sets_t(obs, test_times)
  train <- sets$train
  test <- sets$test
  expect_equal(nrow(obs), nrow(test) + nrow(train))
  expect_equal(class(obs), class(train))
  expect_equal(colnames(obs), colnames(test))
  expect_equal(
    sum(unique(train$time) %in% unique(test$time)),
    0
  ) # means all times are different in each sets
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
  obs <- fread("../testdata/rtp_points.csv")
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
  test_counties <- unique(obs[, county])
  expect_warning(
    create_sets_s(obs[county %in% test_counties, ], test_counties),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  test_counties <- "Durham"
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
  test_net <- "net1"
  expect_error(create_sets_net(obs, test_net), "obs is not a data.table")

  # -- 2nd example: obs is empty
  obs <- data.table()
  expect_error(create_sets_net(obs, test_net), "obs is empty")

  # -- 3rd example: test_net is not a "character"
  test_net <- 3
  obs <- fread("../testdata/rtp_points.csv")
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
  test_net <- unique(obs[, network])
  expect_warning(
    create_sets_net(obs[network %in% test_net, ], test_net),
    "train sample is empty"
  )

  # -- 6th example: sounds good, check test and train dimensions, class
  test_net <- "net1"
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
