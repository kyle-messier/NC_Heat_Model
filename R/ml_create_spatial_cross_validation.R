
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
  
  autoplot(cluster_llo)
  autoplot(block_llo)
  autoplot(net_llo)
  autoplot(rnd_llo)
  
  cluster_llo$type <- "cluster"
  block_llo$type <- "block"
  net_llo$type <- "network"
  rnd_llo$type <- "random"
  
  resamples <-
    dplyr::bind_rows(
      cluster_llo,
      block_llo,
      net_llo,
      rnd_llo
    )
  return(resamples)
}

map_rmse_cv <- function(cv_res) {
  cv_res %>%
    unnest(.preds) %>%
    left_join(cv_rmse, by = c("id", "type")) %>%
    ggplot(aes(color = .estimate)) +
    geom_sf(aes(geometry = geometry), alpha = 1) +
    labs(color = "RMSE") +
    scale_color_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = "ºC"),
      n.breaks = 8,
      guide = guide_legend(reverse = TRUE)
    ) +
    facet_wrap(vars(type), ncol = 1) +
    theme(
      axis.text = element_blank(),
      plot.caption = element_text(size=10, family="serif"),
      legend.text = element_text(size=12, family="serif"),
      legend.title = element_text(size=12, family="serif"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major=element_line(colour="grey")
    )
}


map_res_cv <- function(cv_res) {
  cv_res %>%
    unnest(.preds) %>%
    ggplot(aes(color = tmin - .pred)) +
    geom_sf(aes(geometry = geometry), alpha = 1) +
    labs(color = "RMSE") +
    scale_color_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = "ºC"),
      n.breaks = 8,
      guide = guide_legend(reverse = TRUE)
    ) +
    facet_wrap(vars(type), ncol = 1) +
    theme(
      axis.text = element_blank(),
      plot.caption = element_text(size=10, family="serif"),
      legend.text = element_text(size=12, family="serif"),
      legend.title = element_text(size=12, family="serif"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major=element_line(colour="grey")
    )
}

plot_reg_cv <- function(cv_res){
  cv_res %>%
    unnest(.preds) %>%
    ggplot() +
    geom_point(aes(x = tmin, y = .pred), alpha = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(x = "Observed", y = "Predicted") +
    scale_x_continuous(breaks = seq(9, 27, by = 1)) +
    scale_y_continuous(breaks = seq(9, 27, by = 1)) +
    coord_equal() +
    facet_wrap(vars(type), ncol = 1) +
    theme(
      plot.caption = element_text(size=10, family="serif"),
      legend.text = element_text(size=12, family="serif"),
      legend.title = element_text(size=12, family="serif"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major=element_line(colour="grey")
    )
}
