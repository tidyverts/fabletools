train_null_mdl <- function(.data, ...){
  structure(list(n = NROW(.data), vars = measured_vars(.data)), class = "null_mdl")
}

null_model <- function(formula, ...){
  null_model <- new_model_class("null_mdl", train = train_null_mdl, 
                                specials = new_specials(xreg = function(...) NULL))
  new_model_definition(null_model, formula = !!enquo(formula), ...)
}

#' @export
forecast.null_mdl <- function(object, new_data, ...){
  h <- NROW(new_data)
  construct_fc(rep(NA_real_, h), rep(0, h), dist_unknown(h))
}

#' @export
generate.null_mdl <- function(x, new_data, ...){
  mutate(new_data, .sim = NA_real_)
}

#' @export
stream.null_mdl <- function(object, new_data, ...){
  object$n <- object$n + NROW(new_data)
  object
}

#' @export
refit.null_mdl <- function(object, new_data, ...){
  object$n <- NROW(new_data)
  object
}

#' @export
residuals.null_mdl <- function(object, ...){
  matrix(NA_real_, nrow = object$n, ncol = length(object$vars), 
         dimnames = list(NULL, object$vars))
}

#' @export
fitted.null_mdl <- function(object, ...){
  matrix(NA_real_, nrow = object$n, ncol = length(object$vars), 
         dimnames = list(NULL, object$vars))
}

#' @export
glance.null_mdl <- function(x, ...){
  tibble()
}

#' @export
tidy.null_mdl <- function(x, ...){
  tibble(term = numeric(), estimate = numeric())
}

#' @export
report.null_mdl <- function(object, ...){
  cat("NULL model")
}

#' @export
model_sum.null_mdl <- function(x){
  "NULL model"
}