if (!require(testthat)) install.packages('testthat')
library(testthat)

source("functions_manipulate_spacetime_data.R")

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
  expect_false(any(
    project_dt(dt_eg, crs_ori, crs_dest)$lon_ori - output$lon == 0))
  expect_false(any(
    project_dt(dt_eg, crs_ori, crs_dest)$lat_ori - output$lat == 0))
  
})


test_that("Check add_nc_county works", {
  
  # -- 1st example: datatable is not a data.table
  crs <- "epsg:5070"
  dt_eg <- data.frame("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(add_nc_county(dt_eg, crs), "datatable is not a data.table")
  
  # -- 2nd example: datatable does not contain lon column
  crs <- "epsg:5070"
  dt_eg <- data.table("longitude" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(add_nc_county(dt_eg, crs), 
               "datatable does not contain lon column")
  
  # -- 3rd example: crs is not a character
  crs <- 5070
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  expect_error(add_nc_county(dt_eg, crs), "crs is not a character")
  
  # -- 4th example: test with same crs 
  crs <- "epsg:5070"
  dt_eg <- data.table("lon" = c(1520000, 1650000),
                      "lat" = c(1580000, 1550000))
  output <- add_nc_county(dt_eg, crs)
  expect_contains(colnames(output), 
                  list("lon", "lat", "county"))
  expect_equal(output$county, c("Durham", "Pitt"))
  
  # -- 5th example: test with different crs 
  crs <- "epsg:4326"
  dt_eg <- data.table("lon" = c(-78.895, -77.370),
                      "lat" = c(36.025, 35.599))
  output <- add_nc_county(dt_eg, crs)
  expect_contains(colnames(output), 
                  list("lon", "lat", "county"))
  expect_equal(output$county, c("Durham", "Pitt"))
  
})
