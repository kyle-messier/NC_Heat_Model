#' Create prediction grid with the help of imperviousness raster file
#'
#' @param borders_path a character path to borders shapefile
#' @param covar_path a list of character path to covariates files
#' @param output_path a character path to the output storage folder
#' @returns the same terra::SpatVector with the column "imp"
#' @export
create_pred_rds <- function(borders_path,
                            covar_path,
                            output_path) {
  # create spat raster in borders
  borders <- terra::vect(borders_path)
  imp <- terra::rast(covar_path$imp)
  borders_proj <- terra::project(borders, terra::crs(imp))
  imp <- terra::mask(borders_proj)

  # aggregation at 300m
  pred_rast <- aggregate(imp, fact = 10, fun = "mean")
  names(pred_rast) <- "imp"

  # vector format will also be useful for covariate extraction
  pred_vect <- terra::as.points(pred_rast)

  # ADD SPATIAL COVARIATES
  pred_vect <- add_canopy_h(covar_path$canopy_h, sp_vect = pred_vect) %>%
    add_terrain(covar_path$dem) %>%
    add_tcc(covar_path$dem) %>%
    add_build_fp(covar_path$build_fp) %>%
    add_build_h(covar_path$build_h) %>%
    add_canopy_h(covar_path$canopy_h) %>%
    add_nlcd_ratio(buf_radius = 150,
                   nlcd_path = covar_path$nlcd) %>%
    add_county(county_path = covar_path$county)
  
  # turn into a raster
  pred_rast <- terra::rasterize(pred_vect,
    pred_rast,
    field = names(pred_vect)
  )
  # Terrain covariates
  pred_rast <- add_terrain(pred_rast)
  # Create SpatRasterDataset with all spatial covariates
  pred_rds <- list()
  for (i in names(pred_rast)) {
    pred_rds[[i]] <- pred_rast[[i]]
  }
  pred_rds <- terra::sds(pred_rds)

  # ADD ERA5 COVARIATES
  era5 <- fread(covar_path$era5) %>%
    data.table::rename(time = date)
  # convert era5 to stdt and then to SpatRasterDataset
  era5_stdt <- create_stdtobj(era5, "EPSG:4326")
  era5_rds <- convert_stdt_spatrastdataset(era5_stdt)
  # empty prediction SpatVector
  new_pred_vect <- terra::vect(terra::geom(pred_vect)[, c("x", "y")],
    type = "points",
    crs = terra::crs(pred_vect)
  )
  # extract each daily covariate based on era5 and convert to raster
  pred_rds_era5 <- list()
  for (i in 2:7) {
    pred_rds_era5[[i - 1]] <- terra::project(
      new_pred_vect,
      terra::crs(era5_rds[[i]])
    ) %>%
      terra::extract(x = era5_rds[[i]], bind = TRUE) %>%
      terra::project(terra::crs(new_pred_vect)) %>%
      terra::rasterize(pred_rast, field = terra::names())
  }
  # turn into a SpatRasterDataset
  pred_rds_era5 <- terra::sds(pred_rds_era5)
  terra::names(pred_rds_era5) <- terra::names(era5_rds[[2:7]])

  # SAVE OUTPUT PER DAY AS A .TIF
  dates <- terra::names(pred_rds_era5[[1]])
  era5_covs <- terra::names(pred_rds_era5)
  list_rast_dates <- list()
  for (date in dates) {
    rast_date <- pred_rast
    for (era5_cov in era5_covs) {
      rast_date[[era5_cov]] <- pred_rds_era5[era5_cov][date]
    }
    list_rast_dates[[date]] <- rast_date
    terra::writeRaster(rast_date,
                       filename = paste0(output_path,
                                         "pred_grid_",
                                         date,
                                         ".tif"))
  }
}
