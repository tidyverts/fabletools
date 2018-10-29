#' Refit a model with new data
#' 
#' @param object A model (typically a mable)
#' @param new_data A tsibble containing the new data
#' @param ... Additional optional arguments for refit methods
#' 
#' @export
refit <- function(object, new_data, ...){
  UseMethod("refit")
}

#' @export
refit.mdl_df <- function(object, new_data, ...){
  object %>%
    bind_new_data(new_data) %>% 
    transmute(
      !!!flatten(key(object)),
      model = map2(!!sym("model"), !!sym("new_data"), refit, ...)
    ) %>%
    {suppressWarnings(unnest(., model))} %>% 
    as_mable(key = key(object))
}