library(terra)


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
    stop("Warning: NAs found in imp column")
  }
  return(sp_vect_cov)   
}

add_tcc <- function(tcc_path = "../input/NC_tree-canopy-cover_2021.tif",
                    sp_vect) {
  if (!file.exists(tcc_path)) {
    stop("tcc_path does not exist")
  }
  tcc <- terra::rast(tcc_path)
  sp_vect_cov <- terra::project(sp_vect, crs(tcc)) %>%
    terra::extract(x = tcc, y = ., 
                   fun=function(x) mean(x, na.rm=T), 
                   method='bilinear', bind=T) %>%
    rename(tcc = names(tcc)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$tcc))) {
    stop("Warning: NAs found in tcc column")
  }
  return(sp_vect_cov)   
}
  

add_dem <- function(dem_path = "../input/NC-DEM.tif",
                    sp_vect) {
  if (!file.exists(dem_path)) {
    stop("dem_path does not exist")
  }
  dem <- terra::rast(dem_path)
  sp_vect_cov <- terra::project(sp_vect, crs(dem)) %>%
    terra::extract(x = dem, y = ., bind=T) %>%
    rename(dem = names(dem)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$dem))) {
    stop("Warning: NAs found in dem column")
  }
  return(sp_vect_cov)   
}

add_build_fp <- function(
    build_fp_path = "../input/NC_building-footprints/NorthCarolina_sum.tif",
    sp_vect) {
  if (!file.exists(build_fp_path)) {
    stop("build_fp_path does not exist")
  }
  build_fp <- terra::rast(build_fp_path)
  sp_vect_cov <- terra::project(sp_vect, crs(build_fp)) %>%
    terra::extract(x = build_fp, y = ., 
                   fun=function(x) mean(x, na.rm=T), 
                   method='bilinear', bind=T) %>%
    rename(build_fp = names(build_fp)) %>%
    terra::project(., crs(sp_vect))
  if (any(is.na(sp_vect_cov$build_fp))) {
    stop("Warning: NAs found in build_fp column")
  }
  return(sp_vect_cov)   
}

add_build_h <- function(
    build_h_path = paste0("../input/NC_building-height-by-block/",
                          "NC_building-heights-by-block.shp"),
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
  
  if (all(is.na(sp_vect_cov$build_h) | all(sp_vect_cov$build_h=="NA"))) {
    stop("build_h column is only NA")
  }
  return(sp_vect_cov)   
}


#' Add spatiotemporal covariates to a spatial vector
#' The covariates are not reprojected, only the spatial vector is.
#'
#' @param sp_vect A spatial vector from terra package
#' @returns 
#' @export
add_sptempo_cov <- function(sp_vect) {
  
  crs_ori <- crs(sp_vect)
  
  # open minimum temperatures from era5 reanalysis 
  era5_TNwmo <- rast(paste0("../input/",
                            "era5_daily_reanalysis_20220601_20220831_",
                            "TNwmo.tif"))
  era5_TN7am <- rast(paste0("../input/",
                            "era5_daily_reanalysis_20220601_20220831_",
                            "TN7am.tif"))
  era5_TN12am <- rast(paste0("../input/",
                             "era5_daily_reanalysis_20220601_20220831_",
                             "TN12am.tif"))
                                                
  sp_vect_cov <- project(sp_vect, crs(era5_TNwmo))
  
  return()
}




add_spatial_cov2 <- function(spat_vector) {
  
  # CHECK INPUT
  if (!same.crs(crs(spat_vector), "epsg:5070")) {
    stop("epsg:5070 projection mandatory for area")
  }
  
  # OPEN COVARIATES
  imp <- terra::rast("../input/NC_imperviousness_2019_nad83.tif")
  tcc <- terra::rast("../input/NC_tree-canopy-cover_2021.tif")
  build_fp <- terra::rast(paste0(
    "../input/NC_building-footprints/", 
    "NorthCarolina_sum_nad83.tif"))
  build_h <- terra::vect(paste0(
    "../input/NC_building-height-by-block/", 
    "NC_building-heights-by-block_nad83.shp"))
  dem <- terra::rast("../input/NC-DEM_nad83.tif")
  
  # CHECK COVARIATES CRS
  if (!same.crs(crs(imp), "epsg:5070") |
      !same.crs(crs(tcc), "epsg:5070") |
      !same.crs(crs(build_fp), "epsg:5070") |
      !same.crs(crs(build_h), "epsg:5070") |
      !same.crs(crs(dem), "epsg:5070")) {
    stop("epsg:5070 projection mandatory for area")
  }
  
  # EXTRACT COVARIATES AT POINTS
  output <- terra::extract(imp, output, bind=T) 
  output <- terra::extract(tcc, output, 
                           fun=function(x) mean(x, na.rm=T), 
                           method='bilinear', bind=T)
  output <- terra::extract(build_fp, output, 
                           fun='mean', 
                           method='bilinear', bind=T)
  output <- terra::extract(dem, output, bind=T) 
  
  # -- build_h is a vector and not a raster, bind=T doesn't work...
  output2 <- terra::extract(build_h[,c("Height_cat")], output) 
  output$build_h <- output2$Height_cat
  
  output <- output %>% rename(
    imp = 'NC_imperviousness_2019',
    tcc = 'NC_tree-canopy-cover_2021',
    build_fp = 'NorthCarolina_sum', 
    dem = 'Layer_1'
  )
  
  return(output)
}

