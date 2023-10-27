if (!require(testthat)) install.packages('testthat')
library(testthat)

source("../R/manipulate_spacetime_data.R")


test_that("Check convert_stdt_starray works", {
  
  # -- 1st example: stdt does not have correct columns
  lon <- c(seq(1, 5, 1), seq(1, 5, 1))
  lat <- c(rep(1, 5), rep(3, 5))
  date <- rep(c("t1", "t2"), 5) 
  v1 <- seq(1, 10, 1)
  v2 <- seq(21, 30, 1)
  stdt <- data.table(lon, lat, date, v1, v2)
  expect_error(convert_stdt_starray(stdt), 
               "stdt does not contain lon, lat and time columns")
  
  # -- 2nd example: stdt has duplicates point lon-lat-time
  lon <- c(seq(1, 5, 1), seq(1, 5, 1), 1)
  lat <- c(rep(1, 5), rep(3, 5), 1)
  time <- c(rep(c("t1", "t2"), 5), "t1")
  v1 <- c(seq(1, 10, 1), 1)
  v2 <- c(seq(21, 30, 1), 21)
  stdt <- data.table(lon, lat, time, v1, v2)
  expect_error(convert_stdt_starray(stdt), 
               "stdt contains duplicates in lon-lat-time points")
  
  # -- 3rd example: no covariate in the datatable
  lon <- c(seq(1, 5, 1), seq(1, 5, 1))
  lat <- c(rep(1, 5), rep(3, 5))
  time <- rep(c("t1", "t2"), 5)
  stdt <- data.table(lon, lat, time)
  expect_error(convert_stdt_starray(stdt), 
               "stdt does not contain any variables")
  
  # -- 4th example: everything works fine
  lon <- c(seq(1, 5, 1), seq(1, 5, 1))
  lat <- c(rep(1, 5), rep(3, 5))
  time <- rep(c("t1", "t2"), 5)
  v1 <- seq(1, 10, 1)
  stdt <- data.table(lon, lat, time, v1)
  expect_no_error(convert_stdt_starray(stdt))
  expect_true(all(dim(convert_stdt_starray(stdt)) == c(5, 2, 2, 1)))
  expect_equal(convert_stdt_starray(stdt)[1,1,1,], 1)
  expect_true(is.na(convert_stdt_starray(stdt)[1,2,1,]))
})

test_that("Check convert_starray_stdt works", {
  # -- example: eveything works fine
  lon <- c(seq(1, 5, 1), seq(1, 5, 1))
  lat <- c(rep(1, 5), rep(3, 5))
  time <- rep(c("t1", "t2"), 5)
  v1 <- seq(1, 10, 1)
  v2 <- seq(21, 30, 1)
  stdt <- data.table(lon, lat, time, v1, v2)
  starray <- convert_stdt_starray(stdt)
  # -- dimensional expectations
  expect_equal(Reduce(dim(starray)[1:3], f='*'),
               nrow(convert_starray_stdt(starray)))
})

test_that("Check create_sf works", {
  
  # -- 1st example: datatable is not a data.table
  crs <- "epsg:5070"
  dt_eg <- data.frame("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(create_sf(dt_eg, crs), "datatable is not a data.table")
  
  # -- 2nd example: datatable does not contain lon column
  crs <- "epsg:5070"
  dt_eg <- data.table("longitude" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(create_sf(dt_eg, crs), 
               "datatable does not contain lon column")
  
  # -- 3rd example: crs is not a character
  crs <- 5070
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(create_sf(dt_eg, crs), "crs is not a character")
  
  # -- 4th example: everything sounds good
  crs <- "epsg:4326"
  dt_eg <- data.table("lon" = c(-78.895, -77.370),
                      "lat" = c(36.025, 35.599))
  expect_equal(class(create_sf(dt_eg, crs)), 
               c("sf", "data.table", "data.frame"))

})

test_that("Check create_sftime works", {
  
  # -- 1st example: datatable is not a data.table
  crs <- "epsg:5070"
  dt_eg <- data.frame("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(create_sftime(dt_eg, crs), "datatable is not a data.table")
  
  # -- 2nd example: datatable does not contain lon column
  crs <- "epsg:5070"
  dt_eg <- data.table("longitude" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000),
                      "date" = c("2022-06-01", "2022-06-02"))
  expect_error(create_sftime(dt_eg, crs), 
               "datatable does not contain lon column")
  
  # -- 3rd example: datatable does not contain date column
  crs <- "epsg:5070"
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(create_sftime(dt_eg, crs), 
               "datatable does not contain date column")
  
  # -- 4th example: crs is not a character
  crs <- 5070
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000),
                      "date" = c("2022-06-01", "2022-06-02"))
  expect_error(create_sftime(dt_eg, crs), "crs is not a character")
  
  # -- 5th example: everything sounds good
  crs <- "epsg:4326"
  dt_eg <- data.table("lon" = c(-78.895, -77.370),
                      "lat" = c(36.025, 35.599), 
                      "date" = c("2022-06-01", "2022-06-02"))
  expect_equal(class(create_sftime(dt_eg, crs)), 
               c("sftime", "sf", "data.table", "data.frame"))
  
})

test_that("Check project_dt works", {
  
  # -- 1st example: crs are not characters
  crs_ori <- 4326
  crs_dest <- 5070
  dt_eg <- data.table("lon" = c(-84, -83),
                      "lat" = c(35, 36))
  expect_error(project_dt(dt_eg, crs_ori, crs_dest), 
               "crs are not characters")
  
  # -- 2nd example: datatable does not contain lon column
  crs_ori <- "epsg:4326"
  crs_dest <- "epsg:5070"
  dt_eg <- data.table("longitude" = c(-84, -83),
                      "lat" = c(35, 36))
  expect_error(project_dt(dt_eg, crs_ori, crs_dest), 
               "datatable does not contain lon column")
  
  # -- 3rd example: datatable is not a data.table
  dt_eg <- data.frame("lon" = c(-84, -83),
                      "lat" = c(35, 36))
  expect_error(project_dt(dt_eg, crs_ori, crs_dest), 
               "datatable is not a data.table")
  
  # -- 4th example: everything sounds good
  dt_eg <- data.table("lon" = c(-84, -83),
                      "lat" = c(35, 36))
  expect_equal(class(project_dt(dt_eg, crs_ori, crs_dest)), 
               c("data.table", "data.frame"))
  expect_contains(colnames(project_dt(dt_eg, crs_ori, crs_dest)), 
                  list("lon", "lat", "lon_ori", "lat_ori"))
  expect_false(any(is.na(project_dt(dt_eg, crs_ori, crs_dest)$lon)))
  expect_false(any(is.na(project_dt(dt_eg, crs_ori, crs_dest)$lat)))
  output <- project_dt(dt_eg, crs_ori, crs_dest)
  expect_false(any(output$lon_ori - output$lon == 0))
  expect_false(any(output$lat_ori - output$lat == 0))
  
})



