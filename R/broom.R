#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  x <- transmute(x, aug = map(!!sym("model"), augment))
  unnest(add_class(x, "lst_ts"), key = key(x))
}

#' @export
augment.model <- function(object, ...){
  augment(object$fit, ...)
}

#' @export
glance.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  x <- transmute(x, glanced = map(!!sym("model"), glance))
  unnest(add_class(x, "lst_ts"), key = key(x))
}

#' @export
glance.model <- function(object, ...){
  glance(object$fit, ...)
}

#' @export
tidy.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!(x%@%"models"))
  x <- transmute(x, tidied = map(!!sym("model"), tidy))
  unnest(add_class(x, "lst_ts"), key = key(x))
}

#' @export
tidy.model <- function(object, ...){
  tidy(object$fit, ...)
}
