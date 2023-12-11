#' Project and save imperviousness data to a new crs
#' (used for projection to EPSG:5070 previously to prediction grid creation
#'
#' @param crs_dest A character with the crs of destination
#' @param crs_name A character for the crs without weird characters
#' to concat to file name
project_imp <- function(crs_dest, crs_name) {
  # imperviousness is by default in wgs84 (epsg:4326)
  imp <- rast("../input/NC_imperviousness_2019.tif")
  if (!same.crs(crs(imp), crs_dest)) {
    imp_proj <- project(imp, crs_dest)
    writeRaster(imp_proj,
      filename = paste0(
        "../input/",
        "NC_imperviousness_2019_",
        crs_name,
        ".tif"
      ),
      overwrite = TRUE
    )
  }
}

#' Aggregate digital elevation model (~1m to ~30m)
#'
#' @param dem_path A path to dem file with high resolution
agg_dem <- function(dem_path = "../input/NC-DEM.tif") {
  dem <- rast(dem_path)
  dem_agg <- aggregate(dem, fact = 30, fun = "median")
  writeRaster(dem_agg,
    filename = "../input/NC-DEM-agg.tif",
    overwrite = TRUE
  )
  return(dem_agg)
}
