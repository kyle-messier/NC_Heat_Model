test_that("Check add_imp works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- terra::vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
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
  x <- terra::vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
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
  x <- terra::vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
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
  x <- terra::vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
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
  county_path <- "../testdata/rtp_counties.shp"
  
  # -- 1st example: sp_vect is not a SpatVector
  expect_error(add_county(county_path, 123),
               "data_vect is not a terra::SpatVector.")

  # -- 2nd example: should work
  sp_vect <- terra::vect("../testdata/rtp_spatvector.shp") 
  output <- add_county(county_path, sp_vect)
  expect_contains(names(output), "county")
  expect_equal(output[c(1, 20), ]$county, c("Chatham", "Franklin"))
})

test_that("Check add_build_h works", {
  # -- 1st example: sp_vect erroneous coordinates produce NAs
  d <- data.frame(lon = c(0, 10), lat = c(0, 10))
  x <- terra::vect(d, geom = c("lon", "lat"), crs = "epsg:5070")
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


test_that("Check add_nlcd_ratio works", {
  point_nc1 <- cbind(lon = -79.1, lat = 36.1, var1 = 64) 
  point_nc2 <- cbind(lon = -79, lat = 35.9, var1 = 640)
  eg_data <- rbind(point_nc1, point_nc2) %>%
    as.data.frame() %>%
    terra::vect(crs = "EPSG:4326")
  getwd()
  path_testdata <- "../testdata/rtp_nlcd.tif"
  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  expect_error(
    add_nlcd_ratio(data_vect = eg_data,
                    buf_radius = "1000",
                    nlcd_path = path_testdata),
    "buf_radius is not a numeric."
  )
  # -- buf_radius has likely value
  expect_error(
    add_nlcd_ratio(data_vect = eg_data,
                    buf_radius = -3,
                    nlcd_path = path_testdata),
    "buf_radius has not a likely value."
  )
  # -- data_vect is a SpatVector
  expect_error(
    add_nlcd_ratio(data_vect = 12,
                    nlcd_path = path_testdata),
    "data_vect is not a terra::SpatVector."
  )
  # -- nlcd_path is not a character
  expect_error(
    add_nlcd_ratio(data_vect = eg_data,
                    nlcd_path = 2),
    "nlcd_path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  expect_error(
    add_nlcd_ratio(data_vect = eg_data,
                    nlcd_path = nice_sentence),
    "nlcd_path does not exist."
  )
  
  # CHECK OUTPUT
  buf_radius <- 150
  expect_no_error(add_nlcd_ratio(
    data_vect = eg_data,
    buf_radius = buf_radius,
    nlcd_path = path_testdata
  ))
  output <- add_nlcd_ratio(
    data_vect = eg_data,
    buf_radius = buf_radius,
    nlcd_path = path_testdata
  )
  # -- returns a SpatVector
  expect_equal(class(output)[1], "SpatVector")
  # -- crs is the same than input
  expect_true(terra::same.crs(eg_data, output))
  expect_equal(nrow(output), 2)
  # -- initial names are still in the output SpatVector
  expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  #expect_equal(output$frac_EFO_2021_3000m[1], 0.7940682, tolerance = 1e-7)
  #expect_equal(output$frac_SHB_2021_3000m[2], 0.9987249, tolerance = 1e-7)
  # -- class fraction rows should sum to 1
  expect_equal(rowSums(as.data.frame(output[, 2:ncol(output)])),
               rep(1, 2),
               tolerance = 1e-7
  )
})
