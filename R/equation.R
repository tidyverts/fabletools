#' @export
equation.mdl_df <- function(object, ...){
  if(NROW(object) > 1 || length(mable_vars(object)) > 1){
    abort("Model equations are only supported for individual models. To see the equation for a specific model, use `select()` and `filter()` to identify a single model.")
  }
  equation(object[[(mable_vars(object))[[1]]]][[1]])
}

#' @export
equation.mdl_ts <- function(object, ...){
  if(any(!map_lgl(object$transformation, compose(is.name, body)))){
    abort("Cannot display equations containing transformations.")
  }
  equation(object[["fit"]])
}