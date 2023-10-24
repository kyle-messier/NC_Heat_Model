library(terra)

#' Create prediction grid with several steps:
#' - find urban areas using imperviousness rate
#' - create grid with 200mx200m cells in urban areas 
#' and 1kmx1km cells in rural areas
#' - store the grid in a shapefile with point geometry
#'
#' @param x a Date object
#' @returns a character
create_grid <- function(area, imp) {
  
  # CHECK INPUTS
  
  if (!same.crs(crs(area), crs(imp))) {
    stop("epsg:5070 projection mandatory for area")
  }
  if (!same.crs(crs(imp), "epsg:5070")) {
    stop("epsg:5070 projection mandatory for imp")
  }
  
  # FIND URBAN AREAS WITH HIGH IMPERVIOUSNESS RATE 

  # -- create 5km resolution grid 
  grid_5km <- rast(ext(area), resolution = c(5000, 5000), crs = "epsg:5070")
  grid_5km <- extend(grid_5km, c(1,1))
  grid_5km <- as.polygons(grid_5km)
  grid_5km <- terra::intersect(grid_5km, area)
  
  # -- compute imperviousness mean per 5km x 5km cells
  imp_mean <- zonal(imp, grid_5km, fun='mean', as.raster=T)
  
  # -- create urban and rural masks
  urb_mask <- ifel(imp_mean > 5, 1, NA)
  rur_mask <- ifel(imp_mean <= 5, 1, NA)
  
  # -- convert to grid with 200m resolution for urban areas and 
  # -- 1km for rural areas
  urb_pol <- as.polygons(urb_mask)
  urb_rast <- rast(urb_pol, resolution = c(200, 200), crs = "epsg:5070")
  urb_grid <- as.polygons(urb_rast)
  urb_grid <- terra::intersect(urb_grid, urb_pol)
 
  rur_pol <- as.polygons(rur_mask)
  rur_rast <- rast(rur_pol, resolution = c(1000, 1000), crs = "epsg:5070")
  rur_grid <- as.polygons(rur_rast)
  rur_grid <- terra::intersect(rur_grid, rur_pol)
  
  # CREATE GRID WITH HIGHER RESOLUTION IN URBAN AREAS
  
  urb_grid_df <- centroids(urb_grid, inside=T)
  rur_grid_df <- centroids(rur_grid, inside=T)
  urb_grid_df$geo <- 'urb'
  rur_grid_df$geo <- 'rur'

  grid_points <- rbind(urb_grid_df, rur_grid_df)
  #grid_points <- project(grid_points, "epsg:5070")
  
  # -- keep only the geo covariate
  grid_points <- grid_points %>% select(geo)
  
  # SAVE GRID POINTS AS VECTOR
  
  writeVector(grid_points, 
              "../input/prediction_grid_nad83_empty.shp", 
              overwrite=T)
  return(grid_points)
}
