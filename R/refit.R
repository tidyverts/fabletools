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
refit.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$stage <- "refit"
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)$specials
  object$model$remove_data()
  object$model$stage <- NULL
  
  resp <- map2(seq_along(object$response), object$response, function(i, resp){
    expr(object$transformation[[!!i]](!!resp))
  }) %>% 
    set_names(map_chr(object$response, as_string))
  
  new_data <- transmute(new_data, !!!resp)
  object$fit <- refit(object[["fit"]], new_data, specials = specials, ...)
  object$data <- new_data
  object
}