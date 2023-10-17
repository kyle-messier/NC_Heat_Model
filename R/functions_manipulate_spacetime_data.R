library(gstat)
library(terra)
library(sf)
library(sftime)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(gridExtra)
library(tidyterra)
library(rgeos)
library(data.table) # -- for large flat datasets
library(DT)
library(styler)

#' Create a sf object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the original crs
#' @returns an sf object
create_sf <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (class(crs) != "character") {stop("crs is not a character")}
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  
  data_sf <- st_as_sf(datatable,
                           coords = c("lon", "lat"),
                           remove = F,
                           crs = crs
  )
  return(data_sf)
}


#' Create a sftime object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon", "date"
#' @param crs A character containing the original crs
#' @returns an sftime object
create_sftime <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (class(crs) != "character") {stop("crs is not a character")}
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("date" %in% colnames(datatable))) {
    stop("datatable does not contain date column")
  }
  
  datatable$date <- as.Date(datatable$date)
  data_sft <- st_as_sftime(datatable,
                           coords = c("lon", "lat"),
                           remove = F,
                           crs = crs_ori,
                           time_column_name = "date"
  )
  return(data_sft = data_sft)
}


#' Project coordinates in a datatable from crs_ori to crs_dest
#' 
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs_ori A character containing the original crs of spatial data
#' @param crs_dest A character containing the destination crs of spatial data
#' @returns same datatable object with "lon", "lat", 
#' "lon_ori", "lat_ori" columns
project_dt <- function(datatable, crs_ori, crs_dest) {
  
  if (class(crs_ori) != "character" || class(crs_dest) != "character") {
    stop("crs are not characters") 
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
  
  loc <- unique(datatable[, c("lon", "lat")])
  loc_sf <- create_sf(loc, crs_ori)
  loc_sf <- st_transform(loc_sf, crs_dest)
  colnames(loc_sf)[colnames(loc_sf) == 'lon'] <- 'lon_ori'
  colnames(loc_sf)[colnames(loc_sf) == 'lat'] <- 'lat_ori'
  loc_sf <- loc_sf %>%
    mutate(
      lon = unlist(map(loc_sf$geometry, 1)),
      lat = unlist(map(loc_sf$geometry, 2))
    )
  loc_proj <- as.data.table(loc_sf)
  loc_proj <- loc_proj[, geometry := NULL]
  # renaming is only valid within the function
  colnames(datatable)[colnames(datatable) == 'lon'] <- 'lon_ori'
  colnames(datatable)[colnames(datatable) == 'lat'] <- 'lat_ori'
  datatable_proj <- merge(
    datatable,
    loc_proj,
    by = c("lon_ori", "lat_ori")
  )
  return(datatable_proj)
}


#' Add corresponding NC county to a datatable containing lat, lon
#' 
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the crs of spatial data
#' @returns same datatable object with "county" columns
add_nc_county <- function(datatable, crs) {
  
  if (class(crs) != "character") {stop("crs is not a character")}
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
    }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
    }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  nc_borders <- vect("../input/NC_county_boundary/North_Carolina_State_and_County_Boundary_Polygons.shp")
  if (!same.crs(nc_borders, crs)) {
    nc_borders <- project(nc_borders, crs)
  }
  
  loc <- unique(datatable[, c("lon", "lat")])
  loc <- vect(loc, geom = c("lon", "lat"), crs = crs, keepgeom = TRUE)
  loc$county <- terra::extract(nc_borders[, c("County")], loc)$County
  datatable <- merge(datatable, 
                     loc[, c("lon", "lat", "county")], 
                     by = c("lon", "lat"))
  
  return(datatable)
  
}
