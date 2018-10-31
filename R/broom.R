#' @export
augment.mdl_df <- function(x, ...){
  x %>%
    transmute(
      !!!syms(key_vars(x)),
      aug = map(!!sym("model"), augment)
    ) %>% 
    add_class("lst_ts") %>% 
    unnest(key = key(x))
}

#' @export
glance.mdl_df <- function(x, ...){
  x %>%
    transmute(
      !!!syms(key_vars(x)),
      glanced = map(!!sym("model"), glance)
    ) %>% 
    unnest()
}

#' @export
tidy.mdl_df <- function(x, ...){
  x %>%
    transmute(
      !!!syms(key_vars(x)),
      tidied = map(!!sym("model"), tidy)
    ) %>% 
    unnest()
}