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
  object <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(object)
  object <- transmute(object, !!!keys, !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  unnest(add_class(object, "lst_ts"), key = keys)
}

components.model <- function(object, ...){
  components(object$fit, ...)
}