#' @importFrom stats residuals
#' @export
residuals.mdl_df <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              residuals = map(!!sym("model"), residuals)
    ) %>%
    add_class("lst_ts") %>% 
    unnest(key = keys)
}