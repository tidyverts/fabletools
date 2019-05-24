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
interpolate.model <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$stage <- "interpolate"
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model)$specials,
                       error = function(e){
                         abort(sprintf(
                           "%s
Unable to compute required variables from provided `new_data`.
Does your interpolation data include all variables required by the model?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  
  object$model$remove_data()
  object$model$stage <- NULL
  
  interpolate(object[["fit"]], new_data = new_data, specials = specials, ...)
}