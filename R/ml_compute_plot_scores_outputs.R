#' Displays residual according to prediction
#'
#' @param pred A vector of prediction
#' @param res A vector of residuals
#' @param title A character for plot title
#' @import ggplot2
#' @import tidyr
#' @export
plot_res <- function(pred, res, title) {
  # necessary for linting
  x <- y <- NULL
  ggplot(
    data.frame(x = as.numeric(pred), y = as.numeric(res)),
    aes_string(x, y)
  ) +
    geom_point(col = "blue", alpha = .5) +
    ylim(-10, 10) +
    ylab("Residuals") +
    xlab("Predicted values") +
    ggtitle(title) +
    coord_equal() +
    geom_hline(yintercept = 0, col = "green")
}

#' Displays regression plot
#'
#' @param obs A vector of observed target
#' @param pred A vector of prediction
#' @param title A character for plot title
#' @export
plot_reg <- function(obs, pred, title) {
  x <- y <- NULL
  ggplot(data.frame(x = as.numeric(obs), y = as.numeric(pred)), aes(x, y)) +
    geom_point(col = "blue", alpha = .5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ylab("Predicted") +
    xlab("Observed") +
    ggtitle(title) +
    coord_equal()
}


#' Compute RMSE on cross validation splits
#'
#' @param cv_fit A tibble with 4 columns: splits, id, type and .preds.
#' .preds is also a tibble with 3 columns: geometry, {{variable to predict}}
#' (eg: tmin) and .pred
#' @param predicted The name of the target variable predicted by the model
#' @return a tibble with 5 columns: it, type, .metric, .estimator and .estimate.
#' Each row corresponds to the RMSE of 1 fold for on type of cross validation.
#' @export
compute_rmse_cv <- function(cv_fit, predicted) {
  id <- .preds <- .pred <- type <- NULL
  cv_fit %>%
    unnest(.preds) %>%
    dplyr::group_by(id, type) %>%
    Metrics::rmse({{ predicted }}, .pred)
}

#' map RMSE on cross validation splits
#'
#' @param cv_fit A tibble with 4 columns: splits, id, type and .preds.
#' .preds is also a tibble with 3 columns: geometry, {{variable to predict}}
#' (eg: tmin) and .pred
#' @param cv_rmse cross validation rmse
#' @return ggplot map of RMSE by cross validation type
#' @export
map_rmse_cv <- function(cv_fit, cv_rmse) {
  .preds <- .estimate <- type <- geometry <- NULL
  p <- cv_fit %>%
    unnest(.preds) %>%
    dplyr::left_join(cv_rmse, by = c("id", "type")) %>%
    ggplot(aes(color = .estimate)) +
    geom_sf(aes(geometry = geometry), alpha = 1) +
    labs(color = "RMSE") +
    tidyterra::scale_color_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = "ºC"),
      n.breaks = 8,
      guide = guide_legend(reverse = TRUE)
    ) +
    facet_wrap(vars(type)) +
    my_theme_paper() +
    theme(
      axis.text = element_blank()
    )
  return(p)
}

#' map residuals per monitors
#'
#' @param cv_fit A tibble with 4 columns: splits, id, type and .preds.
#' .preds is also a tibble with 3 columns: geometry, {{variable to predict}}
#' (eg: tmin) and .pred
#' @param predicted The name of the target variable predicted by the model
#' @return ggplot map of residuals after cross validation type
#' @export
map_res_cv <- function(cv_fit, predicted) {
  .preds <- .pred <- type <- geometry <- NULL
  p <- cv_fit %>%
    unnest(.preds) %>%
    ggplot(aes(color = {{ predicted }} - .pred)) +
    geom_sf(aes(geometry = geometry), alpha = 1) +
    labs(color = "Residuals") +
    tidyterra::scale_color_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = "ºC"),
      n.breaks = 8,
      guide = guide_legend(reverse = TRUE)
    ) +
    facet_wrap(vars(type)) +
    my_theme_paper() +
    theme(
      axis.text = element_blank()
    )
  return(p)
}


#' regression plot by type of cross validation
#'
#' @param cv_fit A tibble with 4 columns: splits, id, type and .preds.
#' .preds is also a tibble with 3 columns: geometry, {{variable to predict}}
#' (eg: tmin) and .pred
#' @param predicted The name of the target variable predicted by the model
#' @return regression plot
#' @export
plot_reg_cv <- function(cv_fit, predicted) {
  .preds <- .pred <- type <- NULL
  range <- cv_fit %>%
    unnest(.preds) %>%
    data.table() %>%
    dplyr::summarise(
      min = floor(min({{ predicted }}, .pred)),
      max = ceiling(max({{ predicted }}, .pred))
    )

  p <- cv_fit %>%
    unnest(.preds) %>%
    ggplot() +
    geom_point(aes(x = {{ predicted }}, y = .pred), alpha = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(x = "Observed", y = "Predicted") +
    scale_x_continuous(
      limits = c(range$min, range$max),
      breaks = seq(range$min, range$max, by = 5),
      minor_breaks = seq(range$min, range$max, by = 1)
    ) +
    scale_y_continuous(
      limits = c(range$min, range$max),
      breaks = seq(range$min, range$max, by = 5),
      minor_breaks = seq(range$min, range$max, by = 1)
    ) +
    coord_equal() +
    facet_wrap(vars(type)) +
    my_theme_paper()
  return(p)
}

#' residuals plot by type of cross validation
#'
#' @param cv_fit A tibble with 4 columns: splits, id, type and .preds.
#' .preds is also a tibble with 3 columns: geometry, {{variable to predict}}
#' (eg: tmin) and .pred
#' @param predicted The name of the target variable predicted by the model
#' @return residuals plot
#' @export
plot_res_cv <- function(cv_fit, predicted) {
  .preds <- .pred <- type <- NULL
  range <- cv_fit %>%
    unnest(.preds) %>%
    data.table() %>%
    dplyr::summarise(
      min = floor(min({{ predicted }}, .pred)),
      max = ceiling(max({{ predicted }}, .pred))
    )
  p <- cv_fit %>%
    unnest(.preds) %>%
    ggplot() +
    geom_point(aes(x = .pred, y = {{ predicted }} - .pred), alpha = 1) +
    geom_abline(slope = 0, intercept = 0, color = "red") +
    labs(x = "Predicted values", y = "Residuals") +
    scale_x_continuous(
      limits = c(range$min, range$max),
      breaks = seq(range$min, range$max, by = 5),
      minor_breaks = seq(range$min, range$max, by = 1)
    ) +
    scale_y_continuous(
      limits = c(-10, 10),
      breaks = seq(-10, 10, by = 5),
      minor_breaks = seq(-10, 10, by = 1)
    ) +
    coord_equal() +
    facet_wrap(vars(type)) +
    my_theme_paper()
  return(p)
}
