#' @export
equation.mbl_df <- function(object, ...){
  mdls <- mable_model_cols(object)
  if(NROW(object) != 1 || length(mdls) != 1){
    abort("Model equations are only supported for individual models. To see the equation for a specific model, use `select()` and `filter()` to identify a single model.")
  }
  equation(mdls[[1]][[1]])
}

#' @export
equation.mdl_ts <- function(object, ...){
  if(any(!map_lgl(object$transformation, compose(is.name, body)))){
    abort("Cannot display equations containing transformations.")
  }
  equation(object[["fit"]])
}