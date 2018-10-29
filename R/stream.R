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
    transmute(
      !!!flatten(key(object)),
      model = map2(!!sym("model"), !!sym("new_data"), stream, ...)
    ) %>%
    as_mable(key = key(object))
}