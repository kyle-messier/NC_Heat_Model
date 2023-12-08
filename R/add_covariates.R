#' Add imperviousness covariate to a terra::SpatVector
#'
#' @param imp_path a character with the path to imperviousness raster file
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "imp"
#' @export
add_imp <- function(imp_path, sp_vect) {
  if (!file.exists(imp_path)) {
    stop("imp_path does not exist")
  }
  imp <- terra::rast(imp_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(imp)) %>%
    terra::extract(x = imp, bind = TRUE) %>%
    dplyr::rename(imp = names(imp)) %>%
    terra::project(terra::crs(sp_vect))
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
add_tcc <- function(tcc_path, sp_vect) {
  if (!file.exists(tcc_path)) {
    stop("tcc_path does not exist")
  }
  tcc <- terra::rast(tcc_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(tcc)) %>%
    terra::extract(
      x = tcc,
      fun = function(x) mean(x, na.rm = TRUE),
      method = "bilinear", bind = TRUE
    ) %>%
    dplyr::rename(tcc = names(tcc)) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$tcc))) {
    stop("NAs found in tcc column")
  }
  return(sp_vect_cov)
}

#' Add digital elevation model covariate to a terra::SpatVector
#'
#' @param dem_path a character with the path to digital elevation model file
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "dem"
#' @export
add_dem <- function(dem_path, sp_vect) {
  if (!file.exists(dem_path)) {
    stop("dem_path does not exist")
  }
  dem <- terra::rast(dem_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(dem)) %>%
    terra::extract(x = dem, bind = TRUE) %>%
    dplyr::rename(dem = names(dem)) %>%
    terra::project(terra::crs(sp_vect))
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
add_terrain <- function(rast_dem) {
  if (!("dem" %in% names(rast_dem))) {
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
#' @param canopy_h_path a character with the path to building footprint
#' raster file
#' (default: "../input/NC_building-footprints/NorthCarolina_sum.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_canopy_h <- function(canopy_h_path, sp_vect) {
  if (!file.exists(canopy_h_path)) {
    stop("canopy_h_path does not exist")
  }
  canopy_h <- terra::rast(canopy_h_path)
  canopy_h[canopy_h > 60] <- 0
  sp_vect_cov <- terra::project(sp_vect, terra::crs(canopy_h)) %>%
    terra::extract(
      x = canopy_h,
      fun = function(x) mean(x, na.rm = TRUE),
      method = "bilinear",
      bind = TRUE
    ) %>%
    dplyr::rename(canopy_h = names(canopy_h)) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$canopy_h))) {
    stop("NAs found in canopy_h column")
  }
  return(sp_vect_cov)
}


#' Add building footprint covariate to a terra::SpatVector
#'
#' @param build_fp_path a character with the path to building footprint
#' raster file
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_build_fp <- function(build_fp_path, sp_vect) {
  if (!file.exists(build_fp_path)) {
    stop("build_fp_path does not exist")
  }
  build_fp <- terra::rast(build_fp_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(build_fp)) %>%
    terra::extract(
      x = build_fp,
      fun = function(x) mean(x, na.rm = TRUE),
      method = "bilinear",
      bind = TRUE
    ) %>%
    dplyr::rename(build_fp = names(build_fp)) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$build_fp))) {
    stop("NAs found in build_fp column")
  }
  return(sp_vect_cov)
}


#' Add building height covariate to a terra::SpatVector
#'
#' @param build_h_path a character with the path to building height shapefile
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_h"
#' @export
add_build_h <- function(
    build_h_path, sp_vect) {
  if (!file.exists(build_h_path)) {
    stop("build_h_path does not exist")
  }
  build_h <- terra::vect(build_h_path)
  sp_vect_build_h <- terra::project(sp_vect, terra::crs(build_h)) %>%
    terra::extract(x = build_h[, c("Height_cat")])
  sp_vect_cov <- sp_vect
  sp_vect_cov$build_h <- sp_vect_build_h$Height_cat
  sp_vect_cov <- terra::project(sp_vect_cov, terra::crs(sp_vect))
  if (all(is.na(sp_vect_cov$build_h) | all(sp_vect_cov$build_h == "NA"))) {
    stop("build_h column is only NA")
  }
  return(sp_vect_cov)
}

#' Compute land cover classes ratio in circle buffers arount points
#'
#' @param sp_vect terra::SpatVector of points geometry
#' @param nlcd national land cover dataset as a terra::SpatRaster
#' @param buf_radius numeric giving the radius of buffer around points
#' @export
add_nlcd_class_ratio <- function(nlcd_path, sp_vect, buf_radius = 150) {
  nlcd <- terra::rast(nlcd_path)
  # create circle buffers with 150m radius
  bufs_pol <- terra::buffer(sp_vect, width = buf_radius)
  bufs_pol <- sf::st_as_sf(bufs_pol)
  # crop nlcd raster
  extent <- terra::ext(bufs_pol)
  nlcd_cropped <- terra::crop(nlcd, extent)
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactexctractr::exact_extract(nlcd_cropped,
    sf::st_geometry(bufs_pol),
    fun = "frac",
    stack_apply = TRUE,
    progress = FALSE
  )
  new_sp_vect <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl(
    "frac_",
    names(nlcd_at_bufs)
  )]]
  new_sp_vect <- cbind(sp_vect, new_sp_vect)
  return(new_sp_vect)
}

#' Add corresponding NC county to a datatable containing lat, lon
#'
#' @param county_path Character path to county shp
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the crs of spatial data
#' @returns same datatable object with "county" columns
add_county <- function(county_path, datatable, crs) {
  if (!file.exists(county_path)) {
    stop("county_path does not exist")
  }
  if (class(crs) != "character") {
    stop("crs is not a character")
  }
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  cty_borders <- terra::vect(county_path)
  if (!same.crs(cty_borders, crs)) {
    cty_borders <- terra::project(cty_borders, crs)
  }
  loc <- unique(datatable[, c("lon", "lat")]) %>%
    terra::vect(geom = c("lon", "lat"), crs = crs, keepgeom = TRUE)
  loc$county <- terra::extract(cty_borders[, c("County")], loc)$County
  datatable <- merge(datatable,
    loc[, c("lon", "lat", "county")],
    by = c("lon", "lat")
  )
  return(datatable)
}


#' Add minimum temperature computed from ERA5 reanalysis to SpatVector
#'
#' @param data_vect An stdtobj
#' @param era5_path A character path to era5 csv
#' @returns a SpatVect in long format, each line corresponds to 1 lat-lon-time
#' @export
add_era5_vect <- function(data_vect, era5_path) {
  era5 <- data.table::fread(era5_path) %>%
    dplyr::rename(time = date) %>%
    HeatModel::create_stdtobj(crs_stdt = "EPSG:4326") %>%
    HeatModel::convert_stdt_spatrastdataset()
  # empty prediction SpatVector
  new_data_vect <- terra::vect(geom(data_vect)[, c("x", "y")],
    type = "points",
    crs = terra::crs(data_vect)
  )
  # extract each daily covariate based on era5 and convert to raster
  vect_list <- list()
  for (i in 2:7) {
    vect_list[[i - 1]] <- terra::project(
      new_data_vect,
      terra::crs(era5[[i]])
    ) %>%
      terra::extract(x = era5[[i]], bind = TRUE) %>%
      terra::project(terra::crs(new_data_vect)) %>%
      as.data.frame(xy = TRUE)
  }
  return(vect_list)
}


#' Add minimum temperature computed from ERA5 reanalysis to a SpatRaster
#'
#' @param data_rast A SpatRaster
#' @param era5_path A character path to era5 csv
#' @returns A SpatRasterDataset
#' @export
add_era5_rast <- function(data_rast, era5_path = era5) {
  era5 <- data.table::fread(era5_path) %>%
    dplyr::rename(time = date) %>%
    HeatModel::create_stdtobj(crs_stdt = "EPSG:4326") %>%
    HeatModel::convert_stdt_spatrastdataset()
  data_vect <- terra::as.points(data_rast)
  # empty prediction SpatVector
  new_data_vect <- terra::vect(geom(data_vect)[, c("x", "y")],
    type = "points",
    crs = terra::crs(data_vect)
  )
  # extract each daily covariate based on era5 and convert to raster
  pred_rds_era5 <- list()
  for (i in 2:7) {
    pred_rds_era5[[i - 1]] <- terra::project(
      new_data_vect,
      terra::crs(era5[[i]])
    ) %>%
      terra::extract(x = era5[[i]], bind = TRUE) %>%
      terra::project(terra::crs(new_data_vect)) %>%
      terra::rasterize(data_rast, field = names())
    # maybe names() won't work
  }
  # turn into a SpatRasterDataset
  pred_rds_era5 <- terra::sds(pred_rds_era5)
  names(pred_rds_era5) <- names(era5[[2:7]])
}
