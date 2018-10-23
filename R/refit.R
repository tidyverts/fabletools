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
  key <- syms(key_vars(object))
  object <- bind_new_data(object, new_data)
  names(object)[names(object) == "model"] <- "object"

  out <- map2(object$object, object$new_data, refit, ...) %>% 
    invoke("rbind", .)
  
  add_class(bind_cols(object[map_chr(key, expr_text)], out), class(out))
}