#' @export
augment.mdl_df <- function(x, ...){
  x %>%
    transmute(
      !!!syms(key_vars(x)),
      aug = map(model, augment)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = key(x))
}