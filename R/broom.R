#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  keys <- key(x)
  x <- transmute(x, !!!keys, !!sym(".model"), aug = map(!!sym(".fit"), augment))
  unnest(add_class(x, "lst_ts"), key = keys)
}

#' @export
augment.model <- function(object, ...){
  augment(object$fit, ...)
}

#' @export
glance.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  keys <- key(x)
  x <- transmute(x, !!!keys, !!sym(".model"), glanced = map(!!sym(".fit"), glance))
  unnest(add_class(x, "lst_ts"), key = keys)
}

#' @export
glance.model <- function(object, ...){
  glance(object$fit, ...)
}

#' @export
tidy.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  keys <- key(x)
  x <- transmute(x, !!!keys, !!sym(".model"), tidied = map(!!sym(".fit"), tidy))
  unnest(add_class(x, "lst_ts"), key = keys)
}

#' @export
tidy.model <- function(object, ...){
  tidy(object$fit, ...)
}
