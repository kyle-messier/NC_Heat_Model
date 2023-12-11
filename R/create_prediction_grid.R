#' Create prediction grid with several steps:
#' - find urban areas using imperviousness rate
#' - create grid with 200mx200m cells in urban areas
#' and 1kmx1km cells in rural areas
#' - store the grid in a shapefile with point geometry
#'
#' @param area a borders shapefile with EPSG:5070 projection
#' @param imp imperviousness raster with EPSG:5070 projection
#' @returns a terra::SpatVector of grid points in EPSG:5070 with column geo
#' @export
create_grid <- function(area, imp) {
  # CHECK INPUTS

  if (!terra::same.crs(terra::crs(area), terra::crs(imp))) {
    stop("epsg:5070 projection mandatory for area")
  }
  if (!terra::same.crs(terra::crs(imp), "epsg:5070")) {
    stop("epsg:5070 projection mandatory for imp")
  }

  # FIND URBAN AREAS WITH HIGH IMPERVIOUSNESS RATE

  # -- create 5km resolution grid
  grid_5km <- terra::rast(terra::ext(area),
                          resolution = c(5000, 5000),
                          crs = "epsg:5070")
  grid_5km <- terra::extend(grid_5km, c(1, 1))
  grid_5km <- terra::as.polygons(grid_5km)
  grid_5km <- terra::intersect(grid_5km, area)

  # -- compute imperviousness mean per 5km x 5km cells
  imp_mean <- terra::zonal(imp, grid_5km, fun = "mean", as.raster = TRUE)

  # -- create urban and rural masks
  urb_mask <- terra::ifel(imp_mean > 5, 1, NA)
  rur_mask <- terra::ifel(imp_mean <= 5, 1, NA)

  # -- convert to grid with 200m resolution for urban areas and
  # -- 1km for rural areas
  urb_pol <- terra::as.polygons(urb_mask)
  urb_rast <- terra::rast(urb_pol,
                          resolution = c(200, 200),
                          crs = "epsg:5070")
  urb_grid <- terra::as.polygons(urb_rast)
  urb_grid <- terra::intersect(urb_grid, urb_pol)

  rur_pol <- terra::as.polygons(rur_mask)
  rur_rast <- terra::rast(rur_pol,
                          resolution = c(1000, 1000),
                          crs = "epsg:5070")
  rur_grid <- terra::as.polygons(rur_rast)
  rur_grid <- terra::intersect(rur_grid, rur_pol)

  # CREATE GRID WITH HIGHER RESOLUTION IN URBAN AREAS

  urb_grid_df <- terra::centroids(urb_grid, inside = TRUE)
  rur_grid_df <- terra::centroids(rur_grid, inside = TRUE)
  urb_grid_df$geo <- "urb"
  rur_grid_df$geo <- "rur"

  grid_points <- rbind(urb_grid_df, rur_grid_df)

  # -- keep only the geo covariate
  grid_points <- grid_points %>% dplyr::select("geo")

  # SAVE GRID POINTS AS VECTOR

  terra::writeVector(grid_points,
    "../input/prediction_grid_nad83_empty.shp",
    overwrite = TRUE
  )
  return(grid_points)
}
