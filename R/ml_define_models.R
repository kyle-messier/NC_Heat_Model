
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
#' @return A tibble with columns geometry, tmin and .pred 
compute_preds_lm_cv <- function(sp_samples){
  
  # -- mutate: create .preds col in cv_res tibble
  # -- map: apply to each element of splits
  # -- each split of sp_samples has its own .preds with 
  # -- 3 columns: geometry, tmin and .pred 
  
  cv_fit <- sp_samples %>%
    mutate(.preds = map(splits, compute_preds_lm, formula = formula_q))
  return(cv_fit)
}

