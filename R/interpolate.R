#' Interpolate missing values using a model
#'
#' Fills in the missing values from a model's response variable using the fitted values from the model.
#'
#' @param model a mable
#' @param ... Not used
#'
#' @return
#' A tsibble with missing values interpolated
#'
#' @export
interpolate <- function(model, ...){
  UseMethod("interpolate")
}

#' @export
interpolate.mdl_df <- function(model, new_data, ...){
  if(length(model%@%"models") > 1){
abort("Interpolation can only be done using one model. 
Please use select() to choose the model to interpolate with.")
  }
  model %>%
    bind_new_data(new_data) %>% 
    transmute(
      !!!key(model),
      interpolated = map2(!!!(model%@%"models"), new_data, interpolate, ...)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = key(model))
}

#' @export
interpolate.model <- function(model, ...){
  interpolate(model[["fit"]], ...)
}