if (!require(testthat)) install.packages('testthat')
library(testthat)

test_that("Check projection works", {
  
  crs_ori <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs_dest <- 'PROJCS[\"NAD83 / Conus Albers\",\n    GEOGCS[\"NAD83\",\n        DATUM[\"North_American_Datum_1983\",\n            SPHEROID[\"GRS 1980\",6378137,298.257222101],\n            TOWGS84[0,0,0,0,0,0,0]],\n        PRIMEM[\"Greenwich\",0,\n            AUTHORITY[\"EPSG\",\"8901\"]],\n        UNIT[\"degree\",0.0174532925199433,\n            AUTHORITY[\"EPSG\",\"9122\"]],\n        AUTHORITY[\"EPSG\",\"4269\"]],\n    PROJECTION[\"Albers_Conic_Equal_Area\"],\n    PARAMETER[\"latitude_of_center\",23],\n    PARAMETER[\"longitude_of_center\",-96],\n    PARAMETER[\"standard_parallel_1\",29.5],\n    PARAMETER[\"standard_parallel_2\",45.5],\n    PARAMETER[\"false_easting\",0],\n    PARAMETER[\"false_northing\",0],\n    UNIT[\"metre\",1,\n        AUTHORITY[\"EPSG\",\"9001\"]],\n    AXIS[\"Easting\",EAST],\n    AXIS[\"Northing\",NORTH],\n    AUTHORITY[\"EPSG\",\"5070\"]]'
  dt_eg <- data.table("lon" = c(-84, -83),
                      "lat" = c(35, 36))
  proj_dt_eg <- project_dt(dt_eg, crs_ori, crs_dest)
  
  expect_s4_class(proj_dt_eg, "data.table")
  expect_contains(colnames(proj_dt_eg), 
                  list("lon", "lat", "lon_ori", "lat_ori"))
  expect_false(any(is.na(proj_dt_eg$lon)))
  expect_false(any(is.na(proj_dt_eg$lat)))
  expect_false(any(proj_dt_eg$lon_ori - proj_dt_eg$lon == 0))
  expect_false(any(proj_dt_eg$lat_ori - proj_dt_eg$lat == 0))
  
})