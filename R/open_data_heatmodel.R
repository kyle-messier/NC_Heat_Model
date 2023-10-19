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


#' Open prediction grid for a given period
#'
#' @param period A vector of "Date" objects
#' @returns a data.table of prediction points 
#' (each row corresponds to a lat, lon, date)
open_pred_period <- function(period) {
  
  if (class(period) != "Date") {stop("period is not a Date")}
  
  period <- as.character(period)
  
  for (d in period) {
    file <- paste0(
      "../input/prediction-grid/",
      "prediction_grid_points_urb_rur_space_time_covariates_",
      d,
      ".csv"
    )
    if (!file.exists(file)) {
      stop(paste0("date ", d, " is not in files"))
    }
  }
  
  list_pred <- list()  
  for (d in period) {  
    pred_d <- fread(file)
    list_pred <- append(list_pred, list(pred_d))
  }
  
  pred_p <- rbindlist(list_pred, fill = TRUE)
  return(pred_p = pred_p)
}
