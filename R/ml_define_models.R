#' Fit linear model on a split
#'
#' @param split A tibble of split
#' @return A tibble with columns geometry, tmin and .pred
compute_preds_lm <- function(split, formula) {
  mod <- lm(formula, data = analysis(split))
  holdout <- assessment(split)
  fit <- tibble::tibble(
    geometry = holdout$geometry,
    tmin = holdout$tmin,
    .pred = predict(mod, holdout)
  )
  return(fit)
}


#' Fit linear model on all splits
#'
#' @param sp_sample A tibble with 3 variables: splits, id, type
#' (typically the output of create_sp_folds function)
#' @param formula R formula object
#' @return A tibble with 4 columns: splits, id, type and .preds
#' with .preds a tibble with 3 columns:
#' geometry, {{variable to predict}} and .pred
#' Tip 1: to recover train and test dataframes from split j:
#' as.data.frame(cv_fit$splits[[j]], data="analysis") for train
#' as.data.frame(cv_fit$splits[[j]], data="assessment") for test
#' Equivalently: analysis(cv_fit$splits[[j]]) or assessment(...)
#' When doing head(cv_fit): [n_train/n_test]
#' Tip 2: to access columns of .preds tibble, do
#' cv_fit %>% unnest(.preds)
compute_preds_lm_cv <- function(sp_samples, formula) {
  # -- mutate: create .preds col in cv_res tibble
  # -- map: apply to each element of splits
  # -- each split of sp_samples has its own .preds with
  # -- 3 columns: geometry, {{variable to predict}} (eg: tmin) and .pred
  cv_fit <- sp_samples %>%
    mutate(.preds = purr::map("splits", compute_preds_lm, formula = formula))
  return(cv_fit)
}
