#' Add imperviousness covariate to a terra::SpatVector
#'
#' @param imp_path a character with the path to imperviousness raster file
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "imp"
#' @importFrom magrittr "%>%"
#' @export
add_imp <- function(sp_vect, imp_path) {
  if (!file.exists(imp_path)) {
    stop("imp_path does not exist")
  }
  imp <- terra::rast(imp_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(imp)) %>%
    terra::extract(x = imp, bind = TRUE) %>%
    dplyr::rename(imp = names(imp)) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$imp))) {
    warning("NAs found in imp column")
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
add_tcc <- function(sp_vect, tcc_path) {
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
    warning("NAs found in tcc column")
  }
  return(sp_vect_cov)
}

#' Add digital elevation model covariate to a terra::SpatVector
#'
#' @param dem_path a character with the path to digital elevation model file
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "dem"
#' @export
add_dem <- function(sp_vect, dem_path) {
  if (!file.exists(dem_path)) {
    stop("dem_path does not exist")
  }
  dem <- terra::rast(dem_path)
  sp_vect_cov <- terra::project(sp_vect, terra::crs(dem)) %>%
    terra::extract(x = dem, bind = TRUE) %>%
    dplyr::rename(dem = names(dem)) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$dem))) {
    warning("NAs found in dem column")
  }
  return(sp_vect_cov)
}

#' Add terrain covariates to a terra::SpatRaster (dem included)
#'
#' @param dem_path a path to dem raster
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with terrain covariates
#' (dem, slope, aspect, roughness, flowdir)
#' @export
add_terrain <- function(sp_vect, dem_path) {
  dem_rast <- terra::rast(dem_path)
  names(dem_rast) <- "dem"
  dem_rast$slope <- terra::terrain(dem_rast$dem, "slope")
  # aspect is in degrees, clockwise from North
  # if no slope: 90
  dem_rast$aspect <- terra::terrain(dem_rast$dem, "aspect")
  # Roughness is the difference between the maximum and the
  # minimum value of a cell and its 8 surrounding cells.
  dem_rast$roughness <- terra::terrain(dem_rast$dem, "roughness")
  # Values encoded as power of 2 (x is the cell): IT IS A DISCRETE VARIABLE
  # 32 64 128
  # 16 x   1
  # 8  4  2
  # Cells are set to 0 if no lower neighboring.
  dem_rast$flowdir <- terra::terrain(dem_rast$dem, "flowdir")
  # extract at sp_vect locations
  sp_vect_cov <- terra::project(sp_vect, terra::crs(dem_rast)) %>%
    terra::extract(x = dem_rast, bind = TRUE) %>%
    terra::project(terra::crs(sp_vect))
  if (any(is.na(sp_vect_cov$dem))) {
    warning("NAs found in dem column")
  }
  if (any(is.na(sp_vect_cov$slope))) {
    warning("NAs found in slope column")
  }
  if (any(is.na(sp_vect_cov$aspect))) {
    warning("NAs found in aspect column")
  }
  if (any(is.na(sp_vect_cov$roughness))) {
    warning("NAs found in roughness column")
  }
  if (any(is.na(sp_vect_cov$flowdir))) {
    warning("NAs found in flowdir column")
  }
  return(sp_vect_cov)
}


#' Add forest height
#'
#' @param canopy_h_path a character with the path to building footprint
#' raster file
#' (default: "../input/NC_building-footprints/NorthCarolina_sum.tif")
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_fp"
#' @export
add_canopy_h <- function(sp_vect, canopy_h_path) {
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
    warning("NAs found in canopy_h column")
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
add_build_fp <- function(sp_vect, build_fp_path) {
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
    warning("NAs found in build_fp column")
  }
  return(sp_vect_cov)
}


#' Add building height covariate to a terra::SpatVector
#'
#' @param build_h_path a character with the path to building height shapefile
#' @param sp_vect a terra::SpatVector
#' @returns the same terra::SpatVector with the column "build_h"
#' @export
add_build_h <- function(sp_vect, build_h_path) {
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
    warning("build_h column is only NA")
  }
  return(sp_vect_cov)
}


#' Compute land cover classes ratio in circle buffers around points
#'
#' @param data_vect terra::SpatVector of points geometry
#' @param buf_radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param year numeric giving the year of NLCD data used
#' @param nlcd_path character giving nlcd data path
#' @export
add_nlcd_ratio <- function(data_vect,
                           buf_radius = 150,
                           nlcd_path) {
  # check inputs
  if (!is.numeric(buf_radius)) {
    stop("buf_radius is not a numeric.")
  }
  if (buf_radius <= 0) {
    stop("buf_radius has not a likely value.")
  }
  if (class(data_vect)[1] != "SpatVector") {
    stop("data_vect is not a terra::SpatVector.")
  }
  if (!is.character(nlcd_path)) {
    stop("nlcd_path is not a character.")
  }
  if (!file.exists(nlcd_path)) {
    stop("nlcd_path does not exist.")
  }
  # open nlcd file corresponding to the year
  nlcd <- terra::rast(nlcd_path)
  data_vect_b <- data_vect
  if (!terra::same.crs(data_vect_b, nlcd)) {
    data_vect_b <- terra::project(data_vect_b, terra::crs(nlcd))
  }
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = buf_radius) %>%
    sf::st_as_sf()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactextractr::exact_extract(nlcd,
                                               sf::st_geometry(bufs_pol),
                                               fun = "frac",
                                               stack_apply = TRUE,
                                               progress = FALSE)
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                         names(nlcd_at_bufs))]]
  # change column names
  fpath <- system.file("extdata", "nlcd_classes.csv", package = "HeatModel")
  nlcd_classes <- read.csv(fpath)
  nlcd_names <- names(nlcd_at_bufs) %>%
    sub(pattern = "frac_", replacement = "") %>%
    as.numeric()
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      paste0("frac_", x, "_", buf_radius, "m")
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs) %>%
    terra::project(terra::crs(data_vect))
  return(new_data_vect)
}


#' Add corresponding NC county to a datatable containing lat, lon
#'
#' @param county_path Character path to county shp
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the crs of spatial data
#' @returns same datatable object with "county" columns
add_county <- function(data_vect, county_path) {
  if (!file.exists(county_path)) {
    stop("county_path does not exist")
  }
  if (class(data_vect)[1] != "SpatVector") {
    stop("data_vect is not a terra::SpatVector.")
  }
  cty_borders <- terra::vect(county_path)
  if (!terra::same.crs(cty_borders, data_vect)) {
    cty_borders <- terra::project(cty_borders, data_vect)
  }
  loc <- data_vect
  loc$county <- terra::extract(cty_borders[, c("County")], loc)$County
  return(loc)
}


#' Add minimum temperature computed from ERA5 reanalysis to SpatVector
#'
#' @param data_vect An stdtobj
#' @param era5_path A character path to era5 csv
#' @returns a SpatVect in long format, each line corresponds to 1 lat-lon-time
#' @export
add_era5_vect_old <- function(data_vect, era5_path) {
  era5 <- data.table::fread(era5_path) %>%
    HeatModel::create_stdtobj(crs_stdt = "EPSG:4326") %>%
    HeatModel::convert_stdt_spatrastdataset()
  # empty prediction SpatVector
  new_data_vect <- terra::vect(terra::geom(data_vect)[, c("x", "y")],
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



#' Add minimum temperature computed from ERA5 reanalysis to SpatVector
#'
#' @param data_vect An stdtobj
#' @param era5_path A character path to era5 csv
#' @returns a SpatVect in long format, each line corresponds to 1 lat-lon-time
#' @export
add_era5_vect <- function(data_vect, era5_path) {
  era5 <- data.table::fread(era5_path) %>%
    HeatModel::create_stdtobj(crs_stdt = "EPSG:4326") %>%
    HeatModel::convert_stdt_spatrastdataset()
  # empty prediction SpatVector
  new_data_vect <- terra::vect(terra::geom(data_vect)[, c("x", "y")],
    type = "points",
    crs = terra::crs(data_vect)
  )
  # extract each daily covariate based on era5 and convert to raster
  era5_dt <- list()
  for (i in 2:7) {
    era5_dt[[i - 1]] <- terra::project(
      new_data_vect,
      terra::crs(era5[[i]])
    ) %>%
      terra::extract(x = era5[[i]], bind = TRUE) %>%
      terra::project(terra::crs(new_data_vect)) %>%
      terra::as.data.frame(geom = "XY") %>%
      dplyr::rename("lon" = "x") %>%
      dplyr::rename("lat" = "y") %>%
      data.table::as.data.table() %>%
      data.table::melt(id.vars = c("lon", "lat"),
                       variable.name = "time",
                       value.name = names(era5)[i])
  }
  data_era5 <- cbind(era5_dt[[1]],
                     era5_dt[[2]][, 4],
                     era5_dt[[3]][, 4],
                     era5_dt[[4]][, 4],
                     era5_dt[[5]][, 4],
                     era5_dt[[6]][, 4])
  data_dt <- as.data.frame(data_vect, geom = "XY") %>%
    dplyr::rename("lon" = "x") %>%
    dplyr::rename("lat" = "y") %>%
    data.table::as.data.table()
  data_era5[, "time" := as.factor(time)]
  data_dt[, "time" := as.factor(time)]
  output_vect <- unique(merge(data_dt,
                              data_era5,
                              by = c("lon", "lat", "time"))) %>%
    terra::vect(geom = c("lon", "lat"), crs = terra::crs(new_data_vect))
  return(output_vect)
}


#' Add minimum temperature computed from ERA5 reanalysis to a SpatRaster
#'
#' @param data_rast A SpatRaster
#' @param era5_path A character path to era5 csv
#' @returns A SpatRasterDataset
#' @export
add_era5_rast <- function(data_rast, era5_path) {
  era5 <- data.table::fread(era5_path) %>%
    HeatModel::create_stdtobj(crs_stdt = "EPSG:4326") %>%
    HeatModel::convert_stdt_spatrastdataset()
  data_vect <- terra::as.points(data_rast)
  # empty prediction SpatVector
  new_data_vect <- terra::vect(
    terra::geom(data_vect)[, c("x", "y")],
    type = "points",
    crs = terra::crs(data_vect)
  )
  # extract each daily covariate based on era5 and convert to raster
  pred_rds_era5 <- list()
  for (i in 2:7) {
    names <- names(era5[[i]])
    pred_rds_era5[[i - 1]] <- terra::project(
      new_data_vect,
      terra::crs(era5[[i]])
    ) %>%
      terra::extract(x = era5[[i]], bind = TRUE) %>%
      terra::project(terra::crs(new_data_vect)) %>%
      terra::rasterize(data_rast, field = names)
  }
  # turn into a SpatRasterDataset
  pred_rds_era5 <- terra::sds(pred_rds_era5)
  names(pred_rds_era5) <- names(era5[[2:7]])
  return(pred_rds_era5)
}
