library(terra)
library(data.table)

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
add_dem <- function(dem_path = "../input/NC-DEM-agg.tif",
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

#' Add forest height
#'
#' @param build_fp_path a character with the path to building footprint raster file
#' (default: "../input/NC_building-footprints/NorthCarolina_sum.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_canopy_h <- function(
    canopy_h_path = "../input/NC_forest_height_2019_crs-wgs84.tif",
    sp_vect) {
  if (!file.exists(canopy_h_path)) {
    stop("canopy_h_path does not exist")
  }
  canopy_h <- terra::rast(canopy_h_path)
  canopy_h[canopy_h > 60] <- 0
  sp_vect_cov <- terra::project(sp_vect, crs(canopy_h)) %>%
    terra::extract(
      x = canopy_h, y = .,
      fun = function(x) mean(x, na.rm = T),
      method = "bilinear", bind = T
    ) %>%
    rename(canopy_h = names(canopy_h)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$canopy_h))) {
    stop("NAs found in canopy_h column")
  }
  return(sp_vect_cov)
}


#' Add building footprint covariate to a terra::SpatVector
#'
#' @param build_fp_path a character with the path to building footprint
#' raster file
#' (default: "../input/NC_building-footprints/NorthCarolina_sum.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_build_fp <- function(build_fp_path = "../input/NC_building-footprints/NorthCarolina_sum.tif",
                         sp_vect) {
  
  if (!file.exists(build_fp_path)) {
    stop("build_fp_path does not exist")
  }
  build_fp <- terra::rast(build_fp_path)
  sp_vect_cov <- terra::project(sp_vect, crs(build_fp)) %>%
    terra::extract(
      x = build_fp,
      y = .,
      fun = function(x)
        mean(x, na.rm = T),
      method = "bilinear",
      bind = T
    ) %>%
    rename(build_fp = names(build_fp)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$build_fp))) {
    stop("NAs found in build_fp column")
  }
  return(sp_vect_cov)
}


build_h_table <- as.data.table(list("h_number" = c(1,2,3,4,5,6),
                                    "h_name" = c("Very High", "High", 
                                                 "Medium-High", "Medium",
                                                 "Low-Medium", "Low"))) 


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
  # turn into numeric because rasters use numerics instead of factors
  sp_vect_cov[sp_vect_cov$build_h == "Very High", "build_h"] <- 1
  sp_vect_cov[sp_vect_cov$build_h == "High", "build_h"] <- 2 
  sp_vect_cov[sp_vect_cov$build_h == "Medium-High", "build_h"] <- 3 
  sp_vect_cov[sp_vect_cov$build_h == "Medium", "build_h"] <- 4 
  sp_vect_cov[sp_vect_cov$build_h == "Low-medium", "build_h"] <- 5 
  sp_vect_cov[sp_vect_cov$build_h == "Low", "build_h"] <- 6 
  sp_vect_cov$build_h <- as.numeric(sp_vect_cov$build_h)
  
  if (all(is.na(sp_vect_cov$build_h) | all(sp_vect_cov$build_h == "NA"))) {
    stop("build_h column is only NA")
  }
  return(sp_vect_cov)
}

lc <- rast("../input/NC_nlcd_crs-wgs84.tif")

#' Compute land cover classes ratio in circle buffers arount points
#'
#' @param spvect terra::SpatVector of points geometry
#' @param nlcd national land cover dataset as a terra::SpatRaster
#' @param buf_radius numeric giving the radius of buffer around points 
#' @export
add_nlcd_class_ratio <- function(spvect, nlcd = lc, buf_radius = 150) {
  # create circle buffers with 150m radius 
  bufs_pol <- terra::buffer(spvect, width = buf_radius)
  bufs_pol <- sf::st_as_sf(bufs_pol)
  
  # crop nlcd raster
  extent <- terra::ext(bufs_pol)
  nlcd_cropped <- terra::crop(nlcd, extent)
  
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exact_extract(nlcd_cropped, 
                                st_geometry(bufs_pol), 
                                fun = "frac",
                                stack_apply = T, 
                                progress = F)
  new_spvect <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                       names(nlcd_at_bufs))]]
  new_spvect <- cbind(spvect, new_spvect)
  
  #names(new_spvect) <- sub("frac_", "", names(new_spvect))
  return(new_spvect)
}

#' Add corresponding NC county to a datatable containing lat, lon
#' 
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the crs of spatial data
#' @returns same datatable object with "county" columns
add_nc_county <- function(datatable, crs) {
  
  if (class(crs) != "character") {stop("crs is not a character")}
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  nc_borders <- vect("../input/NC_county_boundary/North_Carolina_State_and_County_Boundary_Polygons.shp")
  if (!same.crs(nc_borders, crs)) {
    nc_borders <- project(nc_borders, crs)
  }
  
  loc <- unique(datatable[, c("lon", "lat")])
  loc <- vect(loc, geom = c("lon", "lat"), crs = crs, keepgeom = TRUE)
  loc$county <- terra::extract(nc_borders[, c("County")], loc)$County
  datatable <- merge(datatable, 
                     loc[, c("lon", "lat", "county")], 
                     by = c("lon", "lat"))
  
  return(datatable)
  
}


#' Add minimum temperature computed from ERA5 reanalysis to ???\
#'
#' @param data An stdtobj
#' @returns ???
#' @export
add_era5_tn <- function(data) {
  stdt <- stdtobj$stdt
  crs_data <- stdtobj$crs_stdt
  
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
