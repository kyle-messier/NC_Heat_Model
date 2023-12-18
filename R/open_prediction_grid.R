#' Open prediction grid for a given period
#'
#' @param period A vector of "Date" objects
#' @param data_path a character to data folder
#' @returns a data.table of prediction points
#' (each row corresponds to a lat, lon, date)
open_pred_period <- function(period, data_path) {
  if (class(period) != "Date") {
    stop("period is not a Date")
  }
  period <- as.character(period)
  for (d in period) {
    file <- paste0(data_path, "pred_grid_", d, ".tif")
    if (!file.exists(file)) {
      stop(paste0("date ", d, " is not in files"))
    }
  }
  list_pred <- list()
  for (d in period) {
    list_pred[[d]] <- terra::rast(file)
  }
  return(list_pred = list_pred)
}
