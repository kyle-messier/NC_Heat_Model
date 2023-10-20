if (!require(spatialsample)) install.packages('spatialsample')
library(spatialsample)
library(yardstick)

#' Create several types of spatial folds for cross validation
#' (with kmeans spatial clusters, by rectangle blocks, by data source, 
#' or randomly in space) 
#'
#' @param data A datatable of observations
#' @param metadata a list of characters with obs info 
#' (eg: county, date, lat, lon, ...)
#' @param predictors a list of characters names of predictors
#' @param predicted a character with the name of the predicted variable
#' @return a tibble object with 3 columns: splits, id and type. 
#' Tips: to recover dataframe from split, use analysis function
#' @export
create_sp_fold <- function(obs_sf) {
  # -- uses k-means clustering
  cluster_llo <- spatial_clustering_cv(obs_sf, v = 10)
  block_llo <- spatial_block_cv(obs_sf, v = 10)
  net_llo <- spatial_leave_location_out_cv(obs_sf,
                                           group = network,
                                           v = 6
  )
  rnd_llo <- spatial_leave_location_out_cv(obs_p_sf, 
                                           group = station,
                                           v = 10
  )
  
  # add leave counties out?
  
  autoplot(cluster_llo)
  autoplot(block_llo)
  autoplot(net_llo)
  autoplot(rnd_llo)
  
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


