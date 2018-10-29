modelsplit <- function(object, ...){
  UseMethod("modelsplit")
}

modelsplit.dable <- function(object, ...){
  modelsplit(object[["decomp"]], ...)
}

#' Extract model or decomposition components
#' 
#' Extracts the decomposed components from an object, or the states from a state space model.
#' 
#' @param object A model or decomposition
#' @param ... Additional arguments passed to methods
#' 
#' @export
components <- function(object, ...){
  UseMethod("components")
}

#' @export
components.mdl_df <- function(object, ...){
  object %>%
    transmute(
      !!!syms(key_vars(object)),
      components = map(object$model, components)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = key(object))
}