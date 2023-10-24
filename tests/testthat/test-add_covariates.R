if (!require(testthat)) install.packages("testthat")
library(testthat)

source("../R/add_covariates.R")

test_that("Check add_imp works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_imp(sp_vect = x), "Warning: NAs found in imp column")

  # -- 2nd example: imp file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(add_imp("blablabla", sp_vect), "imp_path does not exist")

  # -- 3rd example: everything works properly
  expect_no_error(add_imp(sp_vect = sp_vect))
  expect_contains(names(add_imp(sp_vect = sp_vect)), "imp")
})


test_that("Check add_tcc works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_tcc(sp_vect = x), "Warning: NAs found in tcc column")
  
  # -- 2nd example: tcc file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(add_tcc("blablabla", sp_vect), "tcc_path does not exist")
  
  # -- 3rd example: everything works properly
  expect_no_error(add_tcc(sp_vect = sp_vect))
  expect_contains(names(add_tcc(sp_vect = sp_vect)), "tcc")
})


test_that("Check add_dem works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_dem(sp_vect = x), "Warning: NAs found in dem column")
  
  # -- 2nd example: dem file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(add_dem("blablabla", sp_vect), "dem_path does not exist")
  
  # -- 3rd example: everything works properly
  expect_no_error(add_dem(sp_vect = sp_vect))
  expect_contains(names(add_dem(sp_vect = sp_vect)), "dem")
})


test_that("Check add_build_fp works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_build_fp(sp_vect = x), "Warning: NAs found in build_fp column")
  
  # -- 2nd example: build_fp file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(add_build_fp("blablabla", sp_vect), "build_fp_path does not exist")
  
  # -- 3rd example: everything works properly
  expect_no_error(add_build_fp(sp_vect = sp_vect))
  expect_contains(names(add_build_fp(sp_vect = sp_vect)), "build_fp")
})

test_that("Check add_build_h works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_build_h(sp_vect = x), "build_h column is only NA")
  
  # -- 2nd example: build_h file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(add_build_h("blablabla", sp_vect), "build_h_path does not exist")
  
  # -- 3rd example: everything works properly
  expect_no_error(add_build_h(sp_vect = sp_vect))
  expect_contains(names(add_build_h(sp_vect = sp_vect)), "build_h")
})
