#' Refit a mable with new data
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
    gather(".model", ".fit", !!!syms(object%@%"models")) %>% 
    as_tibble %>% 
    transmute(
      !!!key(object),
      !!sym(".model"),
      .fit = map2(!!sym(".fit"), !!sym("new_data"), refit, ...)
    ) %>%
    spread(".model", ".fit") %>% 
    as_mable(key = key(object), models = object%@%"models")
}

#' @export
refit.model <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)$specials
  object$model$remove_data()
  
  object$fit <- refit(object[["fit"]], new_data, specials = specials, ...)
  object$index <- select(new_data, !!index(new_data))
  object
}