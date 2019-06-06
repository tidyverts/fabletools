#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(x%@%"models"))
  kv <- key_vars(x)
  x <- transmute(as_tibble(x),
                 !!!syms(kv), !!sym(".model"), aug = map(!!sym(".fit"), augment))
  unnest_tsbl(x, "aug", parent_key = kv)
}

#' @export
augment.mdl_ts <- function(x, ...){
  tryCatch(augment(x[["fit"]], ...),
           error = function(e){
             idx <- as_string(index(x$data))
             resp <- x$response
             if(length(resp) > 1){
               response(x) %>% 
                 gather(".response", "value", !!!resp, factor_key = TRUE) %>% 
                 left_join(
                   gather(fitted(x, ...), ".response", ".fitted",
                          !!!resp, factor_key = TRUE),
                   by = c(".response", idx)
                 ) %>% 
                 left_join(
                   gather(residuals(x, ...), ".response", ".resid",
                          !!!resp, factor_key = TRUE),
                   by = c(".response", idx)
                 )
             } else {
               set_names(response(x), c(idx, as_string(resp[[1]]))) %>% 
                 left_join(fitted(x, ...), by = idx) %>% 
                 left_join(residuals(x, ...), by = idx)
             }
             
           })
}

#' @export
glance.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(x%@%"models"))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), glanced = map(!!sym(".fit"), glance))
  unnest_tbl(x, "glanced")
}

#' @export
glance.mdl_ts <- function(x, ...){
  glance(x$fit, ...)
}

#' @export
tidy.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(x%@%"models"))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), tidied = map(!!sym(".fit"), tidy))
  unnest_tbl(x, "tidied")
}

#' @export
tidy.mdl_ts <- function(x, ...){
  tidy(x$fit, ...)
}
