#' @export
interpolate.mdl_df <- function(object, new_data, ...){
  if(length(object%@%"models") > 1){
abort("Interpolation can only be done using one model. 
Please use select() to choose the model to interpolate with.")
  }
  kv <- key_vars(object)
  object %>%
    bind_new_data(new_data) %>% 
    as_tibble %>% 
    transmute(
      !!!syms(kv),
      interpolated = map2(!!sym(object%@%"models"), new_data, interpolate, ...)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = kv)
}

#' @export
interpolate.model <- function(object, ...){
  interpolate(object[["fit"]], ...)
}