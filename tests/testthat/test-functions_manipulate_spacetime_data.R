if (!require(testthat)) install.packages('testthat')
library(testthat)

source("../../R/functions_manipulate_spacetime_data.R")

test_that("Check project_dt works", {
  
  crs_ori <- 4326
  crs_dest <- 5070
  dt_eg <- data.table("lon" = c(-84, -83),
                      "lat" = c(35, 36))
  output <- project_dt(dt_eg, crs_ori, crs_dest)
  
  expect_equal(class(output), c("data.table", "data.frame"))
  expect_contains(colnames(output), 
                  list("lon", "lat", "lon_ori", "lat_ori"))
  expect_false(any(is.na(output$lon)))
  expect_false(any(is.na(output$lat)))
  expect_false(any(output$lon_ori - output$lon == 0))
  expect_false(any(output$lat_ori - output$lat == 0))
  
})


test_that("Check add_nc_county works", {
  
  # -- first example: crs is not a character
  crs <- 5070
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(add_nc_county(dt_eg, crs), "crs is not a character")
  
  # -- second example: does not need to project nc_borders
  crs <- "epsg:5070"
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  output <- add_nc_county(dt_eg, crs)
  expect_contains(colnames(output), 
                  list("lon", "lat", "county"))
  expect_equal(output$county, c("Durham", "Pitt"))
  
  # -- third example: need to project nc_borders
  crs <- "epsg:4326"
  dt_eg <- data.table("lon" = c(-78.895, -77.370),
                      "lat" = c(36.025, 35.599))
  output <- add_nc_county(dt_eg, crs)
  expect_contains(colnames(output), 
                  list("lon", "lat", "county"))
  expect_equal(output$county, c("Durham", "Pitt"))
  
})
