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

#' Add terrain covariates to a terra::SpatRaster
#'
#' @param rast_dem a terra::SpatRaster with the column "dem"
#' @returns the same terra::SpatRaster with other terrain covariates
#' @export
add_terrain <- function(rast_dem){
  if(!("dem" %in% names(rast_dem))) {
    stop("dem is not in raster's layers or mispelled.")
  }
  
  rast_dem$slope <- terra::terrain(rast_dem$dem, "slope")
  
  # aspect is in degrees, clockwise from North
  # if no slope: 90
  rast_dem$aspect <- terra::terrain(rast_dem$dem, "aspect")
  
  # Roughness is the difference between the maximum and the
  # minimum value of a cell and its 8 surrounding cells.
  rast_dem$roughness <- terra::terrain(rast_dem$dem, "roughness")
  
  # Values encoded as power of 2 (x is the cell): IT IS A DISCRETE VARIABLE
  # 32 64 128
  # 16 x   1
  # 8  4  2
  # Cells are set to 0 if no lower neighboring.
  rast_dem$flowdir <- terra::terrain(rast_dem$dem, "flowdir")
  return(rast_dem)
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
  #sp_vect_cov[sp_vect_cov$build_h == "Very High", "build_h"] <- 1
  #sp_vect_cov[sp_vect_cov$build_h == "High", "build_h"] <- 2 
  #sp_vect_cov[sp_vect_cov$build_h == "Medium-High", "build_h"] <- 3 
  #sp_vect_cov[sp_vect_cov$build_h == "Medium", "build_h"] <- 4 
  #sp_vect_cov[sp_vect_cov$build_h == "Low-medium", "build_h"] <- 5 
  #sp_vect_cov[sp_vect_cov$build_h == "Low", "build_h"] <- 6 
  #sp_vect_cov$build_h <- as.numeric(sp_vect_cov$build_h)
  
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

era5 <- fread("../input/era5_daily_reanalysis_2022-05-02_2022-09-29.csv")
era5 <- era5 %>% rename(time = date)

# convert era5 to stdt and then to SpatRasterDataset
source("../R/manipulate_spacetime_data.R")
era5_stdt <- create_stdtobj(era5, "EPSG:4326")
era5_rastds <- convert_stdt_spatrastdataset(era5_stdt)

#' Add minimum temperature computed from ERA5 reanalysis to ???\
#'
#' @param data_vect An stdtobj
#' @param era5 A SpatRasterDataset with era5 covariates
#' @returns a SpatVect in long format, each line corresponds to 1 lat-lon-time
#' @export
add_era5_vect <- function(data_vect, era5 = era5_rastds) {
  # empty prediction SpatVector
  new_data_vect <- vect(geom(data_vect)[, c("x", "y")], 
                        type = "points", 
                        crs = crs(data_vect))

  # extract each daily covariate based on era5 and convert to raster
  vect_list <- list()
  for (i in 2:7){
    vect_list[[i-1]] <- terra::project(new_data_vect, 
                                              crs(era5[[i]])) %>%
      terra::extract(x = era5[[i]], y = ., bind = T) %>%
      terra::project(., crs(new_data_vect)) %>%
      as.data.frame(., xy=T)
  }
  return(vect_list)
}


#' Add minimum temperature computed from ERA5 reanalysis to ???\
#'
#' @param data An stdtobj
#' @returns ???
#' @export
add_era5_rast <- function(data_rast, era5 = era5) {
  data_vect <- terra::as.points(data_rast)
  # empty prediction SpatVector
  new_data_vect <- vect(geom(data_vect)[, c("x", "y")], 
                      type = "points", 
                      crs = crs(data_vect))
  
  # extract each daily covariate based on era5 and convert to raster
  pred_rastds_era5 <- list()
  for (i in 2:7){
    pred_rastds_era5[[i-1]] <- terra::project(new_data_vect, 
                                              crs(era5_rastds[[i]])) %>%
      terra::extract(x = era5_rastds[[i]], y = ., bind = T) %>%
      terra::project(., crs(new_data_vect)) %>%
      terra::rasterize(., pred_rast, field = names(.))
  }
  
  # turn into a SpatRasterDataset
  pred_rastds_era5 <- terra::sds(pred_rastds_era5)
  names.(pred_rastds_era5) <- names(era5_rastds[[2:7]])
}
