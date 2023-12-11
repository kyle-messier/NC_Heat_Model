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
  tcc_path <- "../testdata/rtp_tcc.tif"
  expect_error(add_tcc(tcc_path, sp_vect = x), "NAs found in tcc column")

  # -- 2nd example: tcc file does not exist
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp")
  expect_error(add_tcc("blablabla", sp_vect), "tcc_path does not exist")

  # -- 3rd example: everything works properly
  expect_no_error(add_tcc(tcc_path, sp_vect = sp_vect))
  expect_contains(names(add_tcc(tcc_path, sp_vect = sp_vect)), "tcc")
})


test_that("Check add_dem works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  dem_path <- "../testdata/rtp_dem.tif"
  expect_error(add_dem(dem_path, sp_vect = x), "NAs found in dem column")

  # -- 2nd example: dem file does not exist
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp")
  expect_error(add_dem("blablabla", sp_vect), "dem_path does not exist")

  # -- 3rd example: everything works properly
  expect_no_error(add_dem(dem_path, sp_vect = sp_vect))
  expect_contains(names(add_dem(dem_path, sp_vect = sp_vect)), "dem")
})


test_that("Check add_build_fp works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  build_fp_path <- "../testdata/rtp_build_fp.tif"
  expect_error(add_build_fp(build_fp_path, sp_vect = x),
               "NAs found in build_fp column")

  # -- 2nd example: build_fp file does not exist
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp")
  expect_error(
    add_build_fp("blablabla", sp_vect),
    "build_fp_path does not exist"
  )

  # -- 3rd example: everything works properly
  expect_no_error(add_build_fp(build_fp_path, sp_vect = sp_vect))
  expect_contains(names(add_build_fp(build_fp_path,
                                     sp_vect = sp_vect)), "build_fp")
})

test_that("Check add_county works", {
  # -- 1st example: datatable is not a data.table
  crs <- "epsg:5070"
  dt_eg <- data.frame(
    "lon" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  county_path <- "../testdata/rtp_counties.shp"
  expect_error(add_county(county_path, dt_eg, crs),
               "datatable is not a data.table")

  # -- 2nd example: datatable does not contain lon column
  crs <- "epsg:5070"
  dt_eg <- data.table(
    "longitude" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  expect_error(
    add_county(county_path, dt_eg, crs),
    "datatable does not contain lon column"
  )

  # -- 3rd example: crs is not a character
  crs <- 5070
  dt_eg <- data.table(
    "lon" = c(1520000, 1650000),
    "lat" = c(1580000, 1550000)
  )
  expect_error(add_county(county_path, dt_eg, crs),
               "crs is not a character")

  # -- 4th example: test with same crs
  crs <- "epsg:5070"
  dt_eg <- data.table(
    "lon" = c(1510681, 1545833),
    "lat" = c(1574218, 1580651)
  )
  output <- add_county(county_path, dt_eg, crs)
  expect_contains(
    colnames(output),
    list("lon", "lat", "county")
  )
  expect_equal(output$county, c("Orange", "Wake"))

  # -- 5th example: test with different crs
  crs <- "epsg:4326"
  dt_eg <- data.table(
    "lon" = c(-79, -78.6),
    "lat" = c(36, 36)
  )
  output <- add_county(county_path, dt_eg, crs)
  expect_contains(
    colnames(output),
    list("lon", "lat", "county")
  )
  expect_equal(output$county, c("Orange", "Wake"))
})

test_that("Check add_build_h works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
  build_h_path <- "../testdata/rtp_build_h.shp"
  expect_error(add_build_h(build_h_path, sp_vect = x),
               "build_h column is only NA")

  # -- 2nd example: build_h file does not exist
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp")
  expect_error(add_build_h("blablabla", sp_vect),
               "build_h_path does not exist")

  # -- 3rd example: everything works properly
  expect_no_error(add_build_h(build_h_path, sp_vect = sp_vect))
  expect_contains(names(add_build_h(build_h_path,
                                    sp_vect = sp_vect)), "build_h")
})
