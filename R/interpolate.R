#' @export
interpolate.mdl_df <- function(object, new_data, ...){
  if(length(object%@%"models") > 1){
abort("Interpolation can only be done using one model. 
Please use select() to choose the model to interpolate with.")
  }
  keys <- key(object)
  object %>%
    bind_new_data(new_data) %>% 
    as_tibble %>% 
    transmute(
      !!!keys,
      interpolated = map2(!!!(object%@%"models"), new_data, interpolate, ...)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = keys)
}

#' @export
interpolate.model <- function(object, ...){
  interpolate(object[["fit"]], ...)
}