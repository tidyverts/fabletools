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
stream.mable <- function(object, new_data, ...){
  obj_vars <- key_vars(object)
  newdata <- new_data %>% 
    group_by(!!!syms(obj_vars)) %>%
    nest(.key = ".newdata")
  
  if(length(key_vars(object)) == 0){
    object <- object %>%
      mutate(.newdata = newdata$.newdata)
  }
  else{
    object <- object %>%
      left_join(newdata, by = obj_vars)
  }
  object %>%
    mutate(
      model = map2(!!sym("model"), !!sym(".newdata"), stream, ...) %>% add_class("lst_mdl")
    ) %>%
    select(exclude(".newdata")) %>%
    as_mable
}