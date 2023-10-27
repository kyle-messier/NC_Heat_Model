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
library(reshape2)

#' Create a 4d spatiotemporal array from a spatiotemporal datatable
#'
#' @param stdt A data.table object with columns "lat", "lon", "time",
#' and a set of covariates
#' @returns a 4D array with 4 dimensions corresponding to lat, lon, time,
#' variable.
#' To plot the timeserie: plot(starray[i,j,,l])
#' To map one variable at one time: image(starray[,,k,l])
convert_stdt_starray <- function(stdt) {
  if (any(!(c("lon", "lat", "time") %in% colnames(stdt)))) {
    stop("stdt does not contain lon, lat and time columns")
  }

  if (ncol(stdt) < 4) {
    stop("stdt does not contain any variables")
  }

  # check unicity of c(lat, lon, time) among rows
  if (nrow(unique(stdt[, .(lon, lat, time)])) != nrow(stdt)) {
    stop("stdt contains duplicates in lon-lat-time points")
  }
  stdt_complete <- expand_grid(lon = unique(stdt$lon), lat = unique(stdt$lat), time = unique(stdt$time))
  stdt_complete <- merge(stdt, stdt_complete,
    by = c("lon", "lat", "time"),
    all.y = T
  )

  # reshape stdt from wide to long format
  stdt_long <- maditr::melt(stdt_complete, id.vars = c("lon", "lat", "time"))
  # sort columns in this order: variable -> time -> lat -> lon
  setorderv(stdt_long, c("variable", "time", "lat", "lon"))

  dimnames <- lapply(stdt_long[, 1:4], unique)
  starray <- array(stdt_long$value,
    dim = lengths(dimnames),
    dimnames = dimnames
  )
  return(starray)
}

convert_starray_stdt <- function(starray) {
  if (any(!(names(dim(starray)) == c("lon", "lat", "time", "variable")))) {
    stop("starray does not contain lon, lat, time and variable dimension names")
  }

  long_starray <- reshape2::melt(starray)
  wide_starray <- reshape(long_starray,
    idvar = c("lon", "lat", "time"),
    timevar = "variable",
    direction = "wide"
  )
  colnames(wide_starray) <- str_replace(
    colnames(wide_starray),
    "value.",
    ""
  )
  stdt <- as.data.table(wide_starray)
  # stdt <- na.omit(stdt)
  return(stdt)
}


convert_stdt_rastdataset <- function() {

}

#' Create a sf object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the original crs
#' @returns an sf object
create_sf <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (class(crs) != "character") {
    stop("crs is not a character")
  }
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
  if (class(crs) != "character") {
    stop("crs is not a character")
  }
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
    crs = crs,
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
  colnames(loc_sf)[colnames(loc_sf) == "lon"] <- "lon_ori"
  colnames(loc_sf)[colnames(loc_sf) == "lat"] <- "lat_ori"
  loc_sf <- loc_sf %>%
    mutate(
      lon = unlist(map(loc_sf$geometry, 1)),
      lat = unlist(map(loc_sf$geometry, 2))
    )
  loc_proj <- as.data.table(loc_sf)
  loc_proj <- loc_proj[, geometry := NULL]
  # renaming is only valid within the function
  colnames(datatable)[colnames(datatable) == "lon"] <- "lon_ori"
  colnames(datatable)[colnames(datatable) == "lat"] <- "lat_ori"
  datatable_proj <- merge(
    datatable,
    loc_proj,
    by = c("lon_ori", "lat_ori")
  )
  return(datatable_proj)
}
