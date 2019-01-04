#' Extend a fitted model with new data
#'
#' @param object An object (such as a model) which can be extended with additional data
#' @param ... Additional arguments passed on to other methods (usually streamed data and options)
#'
#' @export
stream <- function(object, ...){
  UseMethod("stream")
}

#' @export
stream.mdl_df <- function(object, new_data, ...){
  object %>%
    bind_new_data(new_data) %>% 
    gather(".model", ".fit", !!!(object%@%"models")) %>% 
    transmute(
      !!!key(object),
      !!sym(".model"),
      .fit = map2(!!sym(".fit"), !!sym("new_data"), stream, ...)
    ) %>%
    spread(".model", ".fit") %>% 
    as_mable(key = key(object), models = object%@%"models")
}

#' @export
stream.model <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$data <- new_data
  specials <- parse_model_rhs(object$model)$specials
  object$model$data <- NULL
  
  stream(object[["fit"]], new_data, specials = specials, ...)
}