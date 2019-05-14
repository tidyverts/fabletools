#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(x%@%"models"))
  kv <- key_vars(x)
  x <- transmute(as_tibble(x),
                 !!!syms(kv), !!sym(".model"), aug = map(!!sym(".fit"), augment))
  unnest(add_class(x, "lst_ts"), !!sym("aug"), key = kv)
}

#' @export
augment.model <- function(x, ...){
  tryCatch(augment(x[["fit"]], ...),
           error = function(e){
             idx <- as_string(index(x$data))
             resp <- x$response
             if(length(resp) > 1){
               x$data %>% 
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
               x$data %>% 
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
  unnest(x, !!sym("glanced"))
}

#' @export
glance.model <- function(x, ...){
  glance(x$fit, ...)
}

#' @export
tidy.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(x%@%"models"))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), tidied = map(!!sym(".fit"), tidy))
  unnest(x, !!sym("tidied"))
}

#' @export
tidy.model <- function(x, ...){
  tidy(x$fit, ...)
}
