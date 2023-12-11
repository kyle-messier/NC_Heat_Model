#' Create several types of spatial folds for cross validation
#' (with kmeans spatial clusters, by rectangle blocks, by data source,
#' or randomly in space)
#'
#' @param obs_sf An sf object of observations with columns network and station
#' @return a tibble object with 3 columns: splits, id and type.
#' Tips: to recover train and test dataframes from split j:
#' as.data.frame(sp_samples$splits[[j]], data="analysis") for train
#' as.data.frame(sp_samples$splits[[j]], data="assessment") for test
#' Equivalently: analysis(sp_samples$splits[[j]]) or assessment(...)
#' When doing head(sp_samples): [n_train/n_test]
#' @import spatialsample
#' @export
create_sp_fold <- function(obs_sf) {
  # -- uses k-means clustering
  cluster_llo <- spatialsample::spatial_clustering_cv(obs_sf, v = 10)
  block_llo <- spatialsample::spatial_block_cv(obs_sf, v = 10)
  net_llo <- spatialsample::spatial_leave_location_out_cv(obs_sf,
    group = "network",
    v = 6
  )
  rnd_llo <- spatialsample::spatial_leave_location_out_cv(obs_sf,
    group = "station",
    v = 10
  )
  cluster_llo$type <- "cluster"
  block_llo$type <- "block"
  net_llo$type <- "network"
  rnd_llo$type <- "random"
  sp_samples <-
    dplyr::bind_rows(
      cluster_llo,
      block_llo,
      net_llo,
      rnd_llo
    )
  return(sp_samples)
}
