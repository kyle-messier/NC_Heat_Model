library(terra)

#' Add imperviousness covariate to a terra::SpatVector
#'
#' @param imp_path a character with the path to imperviousness raster file
#' (default: "../input/NC_imperviousness_2019.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "imp"
#' @export
add_imp <- function(imp_path = "../input/NC_imperviousness_2019.tif", sp_vect) {
  if (!file.exists(imp_path)) {
    stop("imp_path does not exist")
  }
  imp <- terra::rast(imp_path)
  sp_vect_cov <- terra::project(sp_vect, crs(imp)) %>%
    terra::extract(x = imp, y = ., bind = T) %>%
    rename(imp = names(imp)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$imp))) {
    stop("NAs found in imp column")
  }
  return(sp_vect_cov)
}

#' Add tree canopy cover covariate to a terra::SpatVector
#'
#' @param tcc_path a character with the path to tree canopy cover raster file
#' (default: "../input/NC_tree-canopy-cover_2021.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "tcc"
#' @export
add_tcc <- function(tcc_path = "../input/NC_tree-canopy-cover_2021.tif",
                    sp_vect) {
  if (!file.exists(tcc_path)) {
    stop("tcc_path does not exist")
  }
  tcc <- terra::rast(tcc_path)
  sp_vect_cov <- terra::project(sp_vect, crs(tcc)) %>%
    terra::extract(
      x = tcc, y = .,
      fun = function(x) mean(x, na.rm = T),
      method = "bilinear", bind = T
    ) %>%
    rename(tcc = names(tcc)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$tcc))) {
    stop("NAs found in tcc column")
  }
  return(sp_vect_cov)
}

#' Add digital elevation model covariate to a terra::SpatVector
#'
#' @param dem_path a character with the path to digital elevation model file
#' (default: "../input/NC-DEM.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "dem"
#' @export
add_dem <- function(dem_path = "../input/NC-DEM.tif",
                    sp_vect) {
  if (!file.exists(dem_path)) {
    stop("dem_path does not exist")
  }
  dem <- terra::rast(dem_path)
  sp_vect_cov <- terra::project(sp_vect, crs(dem)) %>%
    terra::extract(x = dem, y = ., bind = T) %>%
    rename(dem = names(dem)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$dem))) {
    stop("NAs found in dem column")
  }
  return(sp_vect_cov)
}

#' Add building footprint covariate to a terra::SpatVector
#'
#' @param build_fp_path a character with the path to building footprint raster file
#' (default: "../input/NC_building-footprints/NorthCarolina_sum.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_build_fp <- function(
    build_fp_path = "../input/NC_building-footprints/NorthCarolina_sum.tif",
    sp_vect) {
  if (!file.exists(build_fp_path)) {
    stop("build_fp_path does not exist")
  }
  build_fp <- terra::rast(build_fp_path)
  sp_vect_cov <- terra::project(sp_vect, crs(build_fp)) %>%
    terra::extract(
      x = build_fp, y = .,
      fun = function(x) mean(x, na.rm = T),
      method = "bilinear", bind = T
    ) %>%
    rename(build_fp = names(build_fp)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$build_fp))) {
    stop("NAs found in build_fp column")
  }
  return(sp_vect_cov)
}

#' Add building height covariate to a terra::SpatVector
#'
#' @param build_h_path a character with the path to building height shapefile
#' (default: "../input/NC_building-height-by-block/-
#' NC_building-heights-by-block.shp")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_h"
#' @export
add_build_h <- function(
    build_h_path = paste0(
      "../input/NC_building-height-by-block/",
      "NC_building-heights-by-block.shp"
    ),
    sp_vect) {
  if (!file.exists(build_h_path)) {
    stop("build_h_path does not exist")
  }
  build_h <- terra::vect(build_h_path)
  sp_vect_build_h <- terra::project(sp_vect, crs(build_h)) %>%
    terra::extract(x = build_h[, c("Height_cat")], y = .)
  sp_vect_cov <- sp_vect
  sp_vect_cov$build_h <- sp_vect_build_h$Height_cat
  sp_vect_cov <- terra::project(sp_vect_cov, crs(sp_vect))

  if (all(is.na(sp_vect_cov$build_h) | all(sp_vect_cov$build_h == "NA"))) {
    stop("build_h column is only NA")
  }
  return(sp_vect_cov)
}


#' Add minimum temperature computed from ERA5 reanalysis to ???\
#'
#' @param sp_vect A terra::SpatVector
#' @returns ???
#' @export
add_era5TN_cov <- function(sp_vect) {
  crs_ori <- crs(sp_vect)

  # open minimum temperatures from era5 reanalysis
  era5_TNwmo <- rast(paste0(
    "../input/",
    "era5_daily_reanalysis_20220601_20220831_",
    "TNwmo.tif"
  ))
  era5_TN7am <- rast(paste0(
    "../input/",
    "era5_daily_reanalysis_20220601_20220831_",
    "TN7am.tif"
  ))
  era5_TN12am <- rast(paste0(
    "../input/",
    "era5_daily_reanalysis_20220601_20220831_",
    "TN12am.tif"
  ))

  sp_vect_cov <- project(sp_vect, crs(era5_TNwmo))

  return()
}
