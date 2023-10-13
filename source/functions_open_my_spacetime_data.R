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
  period <- as.character(period)
  list_pred <- list()
  for (d in period) {
    pred_d <- fread(
      paste0(
        "../input/prediction-grid/",
        "prediction_grid_points_urb_rur_space_time_covariates_",
        d,
        ".csv"
      )
    )
    list_pred <- append(list_pred, list(pred_d))
  }
  pred_p <- rbindlist(list_pred, fill = TRUE)
  return(pred_p = pred_p)
}
