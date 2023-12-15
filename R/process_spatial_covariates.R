#' Aggregate raster and store it in a new file (useful for dem)
#'
#' @param in_filepath A character path to data file with high resolution
#' @param out_filepath A character path to the folder where the aggregate file
#' will be stored
#' @importFrom magrittr "%>%"
agg_rast <- function(in_filepath, out_filepath, agg_fact = 30) {
  raw <- terra::rast(in_filepath)
  agg <- terra::aggregate(raw, fact = agg_fact, fun = "median") %>%
    terra::writeRaster(
      filename = paste0(out_filepath),
      overwrite = TRUE
    )
  return(agg)
}


#' Subset a polygon area from a SpatRaster or a SpatVector
#'
#' @param sp a SpatRaster or a SpatVector
#' @param poly a SpatVector with polygon geometry
subset_area <- function(sp, poly) {
  poly_proj <- terra::project(poly, terra::crs(sp))
  crop_sp <- terra::crop(sp, poly_proj)
  return(crop_sp)
}
