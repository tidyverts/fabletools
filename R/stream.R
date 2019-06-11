#' Extend a fitted model with new data
#'
#' Extend the length of data used to fit a model and update the parameters to 
#' suit this new data.
#'
#' @param object An object (such as a model) which can be extended with additional data.
#' @param ... Additional arguments passed on to stream methods.
#'
#' @rdname stream
#' @export
stream <- function(object, ...){
  UseMethod("stream")
}

#' @param new_data A dataset of the same structure as was used to fit the model.
#' 
#' @rdname stream
#' @export
stream.mdl_df <- function(object, new_data, ...){
  object %>%
    bind_new_data(new_data) %>% 
    gather(".model", ".fit", !!!syms(object%@%"models")) %>% 
    as_tibble %>% 
    transmute(
      !!!key(object),
      !!sym(".model"),
      .fit = map2(!!sym(".fit"), !!sym("new_data"), stream, ...)
    ) %>%
    spread(".model", ".fit") %>% 
    as_mable(key = key(object), models = object%@%"models")
}

#' @rdname stream
#' @export
stream.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)$specials
  object$model$remove_data()
  
  object$fit <- stream(object[["fit"]], new_data, specials = specials, ...)
  object$data <- rbind(object$data, select(new_data, !!!syms(colnames(object$data))))
  object
}