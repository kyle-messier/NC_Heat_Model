library(testthat)

test_that("Check add_imp works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  imp_path <- "../testdata/rtp_imp.tif"
  expect_error(add_imp(imp_path, sp_vect = x), "NAs found in imp column")

  # -- 2nd example: imp file does not exist
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp")
  expect_error(add_imp("blablabla", sp_vect), "imp_path does not exist")

  # -- 3rd example: everything works properly
  expect_no_error(add_imp(imp_path, sp_vect = sp_vect))
  expect_contains(names(add_imp(imp_path, sp_vect = sp_vect)), "imp")
})


test_that("Check add_tcc works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  expect_error(add_tcc(sp_vect = x), "NAs found in tcc column")

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
  expect_error(add_dem(sp_vect = x), "NAs found in dem column")

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
  expect_error(add_build_fp(sp_vect = x), "NAs found in build_fp column")

  # -- 2nd example: build_fp file does not exist
  sp_vect <- vect("../input/prediction_grid_nad83_empty.shp")
  sp_vect <- sp_vect[600:700]
  expect_error(
    add_build_fp("blablabla", sp_vect),
    "build_fp_path does not exist"
  )

  # -- 3rd example: everything works properly
  expect_no_error(add_build_fp(sp_vect = sp_vect))
  expect_contains(names(add_build_fp(sp_vect = sp_vect)), "build_fp")
})

test_that("Check add_nc_county works", {
  # -- 1st example: datatable is not a data.table
  crs <- "epsg:5070"
  dt_eg <- data.frame(
    "lon" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  expect_error(add_nc_county(dt_eg, crs), "datatable is not a data.table")

  # -- 2nd example: datatable does not contain lon column
  crs <- "epsg:5070"
  dt_eg <- data.table(
    "longitude" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  expect_error(
    add_nc_county(dt_eg, crs),
    "datatable does not contain lon column"
  )

  # -- 3rd example: crs is not a character
  crs <- 5070
  dt_eg <- data.table(
    "lon" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  expect_error(add_nc_county(dt_eg, crs), "crs is not a character")

  # -- 4th example: test with same crs
  crs <- "epsg:5070"
  dt_eg <- data.table(
    "lon" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  output <- add_nc_county(dt_eg, crs)
  expect_contains(
    colnames(output),
    list("lon", "lat", "county")
  )
  expect_equal(output$county, c("Durham", "Pitt"))

  # -- 5th example: test with different crs
  crs <- "epsg:4326"
  dt_eg <- data.table(
    "lon" = c(-78.895, -77.370),
    "lat" = c(36.025, 35.599)
  )
  output <- add_nc_county(dt_eg, crs)
  expect_contains(
    colnames(output),
    list("lon", "lat", "county")
  )
  expect_equal(output$county, c("Durham", "Pitt"))
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


test_that("Check add_tn works", {

})
