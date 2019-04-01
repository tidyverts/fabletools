#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), aug = map(!!sym(".fit"), augment))
  unnest(add_class(x, "lst_ts"), !!sym("aug"), key = !!keys)
}

#' @export
augment.model <- function(x, ...){
  augment(x$fit, ...)
}

#' @export
glance.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
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
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), tidied = map(!!sym(".fit"), tidy))
  unnest(x, !!sym("tidied"))
}

#' @export
tidy.model <- function(x, ...){
  tidy(x$fit, ...)
}
