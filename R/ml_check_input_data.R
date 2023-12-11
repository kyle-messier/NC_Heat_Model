#' Check observation dataset content
#'
#' @param data A datatable of observations
#' @param metadata a list of characters with obs info
#' (eg: county, date, lat, lon, ...)
#' @param predictors a list of characters names of predictors
#' @param predicted a character with the name of the predicted variable
#' @import emojifont
#' @export
check_obs <- function(data, metadata, predictors, predicted) {
  if (sum(!(metadata %in% colnames(data))) != 0) {
    stop("some metadata columns are missing")
  }
  if (sum(!(predictors %in% colnames(data))) != 0) {
    stop("some predictors columns are missing")
  }
  if (!(predicted %in% colnames(data)) != 0) {
    stop("predicted variable is missing")
  }
  message("observations content: ", emoji("white_check_mark"))
}

#' Check prediction grid content
#'
#' @param data A datatable of observations
#' @param metadata a list of characters with obs info
#' (eg: county, date, lat, lon, ...)
#' @param predictors a list of characters names of predictors
#' @import emojifont
#' @export
check_pred_grid <- function(data, metadata, predictors) {
  if (sum(!(metadata %in% colnames(data))) != 0) {
    stop("some metadata columns are missing")
  }
  if (sum(!(predictors %in% colnames(data))) != 0) {
    stop("some predictors columns are missing")
  }
  message("prediction grid content: ", emoji("white_check_mark"))
}
