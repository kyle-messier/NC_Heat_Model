
test_that("Check check_obs works", {
  
  metadata <- c("lon", "lat", "date")
  predictors <- c("dem", "imp")
  predicted <- "tmin"
  
  # 1st example: some metadata columns missing 
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), 
                    dem=as.numeric(), imp=as.numeric(), 
                    tmin=as.numeric())
  expect_error(check_obs(obs, metadata, predictors, predicted), 
               "some metadata columns are missing")
  
  # 2nd example: some predictors columns missing
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), date=as.numeric(),
                    imp=as.numeric(), 
                    tmin=as.numeric())
  expect_error(check_obs(obs, metadata, predictors, predicted), 
               "some predictors columns are missing")
  
  # 3rd example: predicted variable missing
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), date=as.numeric(),
                    dem=as.numeric(), imp=as.numeric())
  expect_error(check_obs(obs, metadata, predictors, predicted), 
               "predicted variable is missing")
  
  # 4th example: everything works fine
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), date=as.numeric(),
                    dem=as.numeric(), imp=as.numeric(), 
                    tmin=as.numeric())
  expect_no_error(check_obs(obs, metadata, predictors, predicted))
  expect_message(check_obs(obs, metadata, predictors, predicted),
                 message("observations content: ", emoji("white_check_mark")))
  
})

test_that("Check check_pred_grid works", {
  
  metadata <- c("lon", "lat", "date")
  predictors <- c("dem", "imp")
  
  # 1st example: some metadata columns missing 
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), 
                    dem=as.numeric(), imp=as.numeric())
  expect_error(check_pred_grid(obs, metadata, predictors), 
               "some metadata columns are missing")
  
  # 2nd example: some predictors columns missing
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), date=as.numeric(),
                    imp=as.numeric())
  expect_error(check_pred_grid(obs, metadata, predictors), 
               "some predictors columns are missing")
  
  # 3rd example: everything works fine
  obs <- data.table(lon=as.numeric(), lat=as.numeric(), date=as.numeric(),
                    dem=as.numeric(), imp=as.numeric())
  expect_no_error(check_pred_grid(obs, metadata, predictors))
  expect_message(check_pred_grid(obs, metadata, predictors),
                 message("prediction grid content: ", emoji("white_check_mark")))
  
})
