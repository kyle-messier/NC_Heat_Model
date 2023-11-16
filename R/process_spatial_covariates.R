library(terra)

#' Project and save imperviousness data to a new crs 
#' (used for projection to EPSG:5070 previously to prediction grid creation 
#'
#' @param crs_dest A character with the crs of destination 
#' @param crs_name A character for the crs without weird characters 
#' to concat to file name
project_imp <- function(crs_dest, crs_name) {
  
  # -- imperviousness is by default in wgs84 (epsg:4326)
  imp <- rast("../input/NC_imperviousness_2019.tif")
  if (! same.crs(crs(imp), crs_dest)) {
    imp_proj <- project(imp, crs_dest)
    writeRaster(imp_proj,
                filename = paste0(
                  "../input/", 
                  "NC_imperviousness_2019_",
                  crs_name, ".tif"
                ), overwrite = T
    )
  }
}

#' Compute land cover classes ratio in circle buffers arount points
#'
#' @param spvect terra::SpatVector of points geometry
#' @param nlcd national land cover dataset as a terra::SpatRaster
#' @param buf_radius numeric giving the radius of buffer around points 
#' @export
compute_ndld_class_ratio <- function(spvect, nlcd, buf_radius = 150) {
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
                                stack_apply = T)
  
  new_spvect <- cbind(spvect, nlcd_at_bufs)
  new_spvect <- new_spvect[names(new_spvect)[grepl("frac_",
                                                   names(new_spvect))]]
  names(new_spvect) <- sub("frac_", "", names(new_spvect))
  return(new_spvect)
}