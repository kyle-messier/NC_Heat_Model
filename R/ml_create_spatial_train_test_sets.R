#' Create train and test dataset
#' by randomly sampling in space and time
#'
#' @param obs A datatable of observations
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, time)
create_sets_rndst <- function(obs) {
  if (!("data.table" %in% class(obs))) {
    stop("obs is not a data.table")
  }
  if (nrow(obs) == 0) {
    stop("obs is empty")
  }
  obs <- na.omit(obs)
  test_ratio <- .2
  npop <- nrow(obs)
  ntest <- ceiling(npop * test_ratio)
  test_i <- sample(1:npop, ntest)
  train_i <- setdiff(1:npop, test_i)
  train <- obs[train_i, ]
  test <- obs[test_i, ]
  return(list(train = train, test = test))
}


#' Create train and test dataset
#' by randomly sampling in space
#'
#' @param obs A datatable of observations
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, time)
create_sets_rnds <- function(obs) {
  station <- NULL
  if (!("data.table" %in% class(obs))) {
    stop("obs is not a data.table")
  }
  if (nrow(obs) == 0) {
    stop("obs is empty")
  }
  if (!("station" %in% colnames(obs))) {
    stop("station is not in obs colnames")
  }
  test_ratio <- .2
  monitors <- unique(obs$station)
  n_mon <- length(monitors)
  ntest <- ceiling(n_mon * test_ratio)
  test_i <- sample(1:n_mon, ntest)
  train_i <- setdiff(1:n_mon, test_i)
  train <- obs[station %in% monitors[train_i], ]
  test <- obs[station %in% monitors[test_i], ]
  return(list(train = train, test = test))
}


#' Create train and test dataset
#' by removing specific times
#'
#' @param obs A datatable of observations
#' @param test_times A vector of times
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, time)
create_sets_t <- function(obs, test_times) {
  if (!("data.table" %in% class(obs))) {
    stop("obs is not a data.table")
  }
  if (nrow(obs) == 0) {
    stop("obs is empty")
  }
  if (!("time" %in% colnames(obs))) {
    stop("time is not in obs colnames")
  }
  if (class(test_times) != "Date") {
    stop("test_times is not a Date")
  }
  if (sum(!(test_times %in% unique(obs$time))) != 0) {
    stop("some of test_times are not in obs")
  }
  train <- obs[!(time %in% test_times), ]
  test <- obs[time %in% test_times, ]
  if (nrow(train) == 0) {
    warning("train sample is empty")
  }
  return(list(train = train, test = test))
}

#' Create train and test dataset
#' by removing specific counties
#'
#' @param obs A datatable of observations
#' @param test_counties A vector of county names
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, time)
create_sets_s <- function(obs, test_counties) {
  county <- NULL
  if (!("data.table" %in% class(obs))) {
    stop("obs is not a data.table")
  }
  if (nrow(obs) == 0) {
    stop("obs is empty")
  }
  if (class(test_counties) != "character") {
    stop("test_counties is not a character or is empty")
  }
  if (sum(!(test_counties %in% unique(obs$county))) != 0) {
    stop("some of test_counties are not in obs")
  }
  train <- obs[!(county %in% test_counties), ]
  test <- obs[county %in% test_counties, ]
  if (nrow(train) == 0) {
    warning("train sample is empty")
  }
  return(list(train = train, test = test))
}

#' Create train and test dataset
#' by removing specific monitor networks
#'
#' @param obs A datatable of observations
#' @param test_net A vector of network names
#' @returns a list with train and test datatables
#' (each row corresponds to a lat, lon, time)
create_sets_net <- function(obs, test_net) {
  network <- NULL
  if (!("data.table" %in% class(obs))) {
    stop("obs is not a data.table")
  }
  if (nrow(obs) == 0) {
    stop("obs is empty")
  }
  if (class(test_net) != "character") {
    stop("test_net is not a character or is empty")
  }
  if (sum(!(test_net %in% unique(obs$network))) != 0) {
    stop("some of test_net are not in obs")
  }

  train <- obs[!(network %in% test_net), ]
  test <- obs[network %in% test_net, ]

  if (nrow(train) == 0) {
    warning("train sample is empty")
  }

  return(list(train = train, test = test))
}
