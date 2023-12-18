#' Create prediction grid with the help of imperviousness raster file
#'
#' @param borders_path a character path to borders shapefile
#' @param covar_files_list a list of character path to covariates files
#' @param output_path a character path to the output storage folder
#' @returns the same terra::SpatVector with the column "imp"
#' @export
create_pred_rds <- function(borders_path,
                            covar_files_list,
                            output_path) {
  # create spat raster in borders
  borders <- terra::vect(borders_path)
  imp <- terra::rast(covar_files_list$imp)
  borders_proj <- terra::project(borders, terra::crs(imp))
  imp <- terra::mask(imp, borders_proj)

  # aggregation at 300m
  pred_rast <- terra::aggregate(imp, fact = 10, fun = "mean")
  names(pred_rast) <- "imp"

  # vector format will also be useful for covariate extraction
  pred_vect <- terra::as.points(pred_rast)

  # ADD SPATIAL COVARIATES
  pred_vect <- add_canopy_h(covar_files_list$canopy_h, sp_vect = pred_vect) %>%
    add_terrain(covar_files_list$dem) %>%
    add_tcc(covar_files_list$tcc) %>%
    add_build_fp(covar_files_list$build_fp) %>%
    add_build_h(covar_files_list$build_h) %>%
    add_nlcd_ratio(buf_radius = 150,
                   nlcd_path = covar_files_list$nlcd) %>%
    add_county(county_path = covar_files_list$county)
  # turn into a raster
  pred_rast <- terra::rasterize(pred_vect,
    pred_rast,
    field = names(pred_vect)
  )
  # Create SpatRasterDataset with all spatial covariates
  pred_rds <- list()
  for (i in names(pred_rast)) {
    pred_rds[[i]] <- pred_rast[[i]]
  }
  pred_rds <- terra::sds(pred_rds)

  # ADD ERA5 COVARIATES
  pred_rds_era5 <- add_era5_rast(pred_rast, era5_path = covar_files_list$era5)
  # SAVE OUTPUT PER DAY AS A .TIF
  dates <- names(pred_rds_era5[[1]])
  era5_covs <- names(pred_rds_era5)
  list_rast_dates <- list()
  for (date in dates) {
    rast_date <- pred_rast
    for (era5_cov in era5_covs) {
      rast_date[[era5_cov]] <- pred_rds_era5[[era5_cov]][[date]]
    }
    list_rast_dates[[date]] <- rast_date
    terra::writeRaster(rast_date,
                       filename = paste0(output_path,
                                         "pred_grid_",
                                         date,
                                         ".tif"),
                       overwrite = TRUE)
  }
}
