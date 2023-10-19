if (!require(emojifont)) install.packages('emojifont')
library(emojifont)

#' Check observation dataset content
#'
#' @param data A datatable of observations
#' @param metadata a list of characters with obs info 
#' (eg: county, date, lat, lon, ...)
#' @param predictors a list of characters names of predictors
#' @param predicted a character with the name of the predicted variable
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
check_pred_grid <- function(data, metadata, predictors) {
  if (sum(!(metadata %in% colnames(data))) != 0) {
    stop("some metadata columns are missing")
  }
  if (sum(!(predictors %in% colnames(data))) != 0) {
    stop("some predictors columns are missing")
  }
  message("prediction grid content: ", emoji("white_check_mark"))
}



# Train and test processing

#' Create train and test dataset
#' by randomly sampling in space and time
#'
#' @param obs A datatable of observations
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, date)
create_sets_rndst <- function(obs) {
  
  if (!("data.table" %in% class(obs))) {stop("obs is not a data.table")}
  if (nrow(obs) == 0) {stop("obs is empty")}
  
  obs <- na.omit(obs)
  test_ratio <- .2
  npop <- nrow(obs)
  
  ntest <- ceiling(npop * test_ratio)
  test_i <- sample(1:npop, ntest)
  train_i <- setdiff(1:npop, test_i)
  train <- obs[train_i, ]
  test <- obs[test_i, ]
  return(list(train=train, test=test))
}


#' Create train and test dataset
#' by randomly sampling in space 
#'
#' @param obs A datatable of observations
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, date)
create_sets_rnds <- function(obs) {
  
  if (!("data.table" %in% class(obs))) {stop("obs is not a data.table")}
  if (nrow(obs) == 0) {stop("obs is empty")}
  if (!("station" %in% colnames(obs))) {stop("station is not in obs colnames")}
  
  test_ratio <- .2
  monitors <- unique(obs$station)
  n.mon <- length(monitors)
  ntest <- ceiling(n.mon * test_ratio)
  test_i <- sample(1:n.mon, ntest)
  train_i <- setdiff(1:n.mon, test_i)
  train <- obs[station %in% monitors[train_i], ]
  test <- obs[station %in% monitors[test_i], ]
  
  return(list(train=train, test=test))
}


#' Create train and test dataset
#' by removing specific dates 
#'
#' @param obs A datatable of observations
#' @param test_dates A vector of dates
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, date)
create_sets_t <- function(obs, test_dates) {
  
  if (!("data.table" %in% class(obs))) {stop("obs is not a data.table")}
  if (nrow(obs) == 0) {stop("obs is empty")}
  if (!("date" %in% colnames(obs))) {stop("date is not in obs colnames")}
  if (class(test_dates) != "Date") {stop("test_dates is not a Date")}
  if (sum(!(test_dates %in% unique(obs$date))) != 0) {
    stop("some of test_dates are not in obs")
    }
  
  train <- obs[!(date %in% test_dates), ]
  test <- obs[date %in% test_dates, ]
  
  if (nrow(train) == 0) {warning("train sample is empty")}
  
  return(list(train=train, test=test))
}

#' Create train and test dataset
#' by removing specific counties
#'
#' @param obs A datatable of observations
#' @param test_counties A vector of county names
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, date)
create_sets_s <- function(obs, test_counties) {
  
  if (!("data.table" %in% class(obs))) {stop("obs is not a data.table")}
  if (nrow(obs) == 0) {stop("obs is empty")}
  if (class(test_counties) != "character") {
    stop("test_counties is not a character or is empty")}
  if (sum(!(test_counties %in% unique(obs$county))) != 0) {
    stop("some of test_counties are not in obs")
  }
  
  train <- obs[!(county %in% test_counties), ]
  test <- obs[county %in% test_counties, ]
  
  if (nrow(train) == 0) {warning("train sample is empty")}
  
  return(list(train=train, test=test))
}

#' Create train and test dataset
#' by removing specific monitor networks
#'
#' @param obs A datatable of observations
#' @param test_net A vector of network names
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, date)
create_sets_net <- function(obs, test_net) {
  
  if (!("data.table" %in% class(obs))) {stop("obs is not a data.table")}
  if (nrow(obs) == 0) {stop("obs is empty")}
  if (class(test_net) != "character") {
    stop("test_net is not a character or is empty")}
  if (sum(!(test_net %in% unique(obs$network))) != 0) {
    stop("some of test_net are not in obs")
  }
  
  train <- obs[!(network %in% test_net), ]
  test <- obs[network %in% test_net, ]
  
  if (nrow(train) == 0) {warning("train sample is empty")}
  
  return(list(train=train, test=test))
}

#' plot residual according to prediction
#'
#' @param pred A vector of prediction
#' @param res A vector of residuals
#' @param title A character for plot title
plot_res <- function(pred, res, title) {
  ggplot(data.frame(x = as.numeric(pred), y = as.numeric(res)), aes(x, y)) +
    geom_point(col = "blue", alpha = .5) +
    ylim(-10, 10) +
    ylab("Residuals") +
    xlab("Predicted values") +
    ggtitle(title) +
    coord_equal() +
    geom_hline(yintercept = 0, col = "green")
}

#' regression plot
#'
#' @param pred A vector of prediction
#' @param res A vector of residuals
#' @param title A character for plot title
plot_reg <- function(obs, pred, title) {
  ggplot(data.frame(x = as.numeric(obs), y = as.numeric(pred)), aes(x, y)) +
    geom_point(col = "blue", alpha = .5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ylab("Predicted") +
    xlab("Observed") +
    ggtitle(title) +
    coord_equal() 
}


