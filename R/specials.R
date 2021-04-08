#' Create evaluation environment for specials
#' 
#' Allows extension packages to make use of the formula parsing of specials.
#' 
#' @param ... A named set of functions which used to parse formula inputs
#' @param .required_specials The names of specials which must be provided (and if not, are included with no inputs).
#' @param .xreg_specials The names of specials which will be only used as inputs to other specials (most commonly `xreg`).
#' 
#' @export
new_specials <- function(..., .required_specials = NULL, .xreg_specials = NULL){
  specials <- squash(list2(...))
  if(is.null(specials$xreg)){
    specials$xreg <- function(...) abort(sprintf("Exogenous regressors are not supported for %s.", self$model))
  }
  structure(specials,
            required_specials = .required_specials,
            xreg_specials = .xreg_specials,
            class="fable_specials")
}

#' Special for producing a model matrix of exogenous regressors
#' 
#' @param ... Arguments for `fable_xreg_matrix` (see Details)
#' 
#' @details 
#' 
#' Currently the `fable_xreg_matrix` helper supports a single argument named
#' `default_intercept`. If this argument is TRUE (passed via `...` above), then
#' the intercept will be returned in the matrix if not specified (much like the
#' behaviour of `lm()`). If FALSE, then the intercept will only be included if
#' explicitly requested via `1` in the formula.
#' 
#' @importFrom stats model.frame model.matrix terms
#' @export
special_xreg <- function(...) {
  new_function(
    args = pairlist2(...=),
    body = call2(call2(":::", sym("fabletools"), sym("fable_xreg_matrix")),
                 sym("..."), ..., .data = parse_expr("self$data")),
    env = base_env()
  )
}

fable_xreg_matrix <- function(..., .data, default_intercept = TRUE) {
  # Remove default intercept if needed.
  dots <- enexprs(...)
  if(!default_intercept) {
    constants <- map_lgl(dots, inherits, "numeric")
    constant_specified <- any(map_lgl(dots[constants], `%in%`, c(-1, 0, 1)))
    # If the constant isn't specified, remove it.
    if(!constant_specified) dots <- c(dots, list(0))
  }
  
  # Combine `...` into a model formula, then evaluate terms() to substitute `.`
  model_formula <- new_formula(
    lhs = NULL,
    rhs = reduce(dots, function(.x, .y) call2("+", .x, .y))
  )
  model_formula <- terms(model_formula, data = .data)
  
  # Produce appropriate evaluation environment with specials
  env <- map(enquos(...), get_env)
  env[map_lgl(env, compose(is_empty, env_parents))] <- NULL
  env <- if (!is_empty(env)) get_env(env[[1]]) else base_env()
  
  # Produce xreg matrix
  xreg <- model.frame(model_formula, data = env, na.action = stats::na.pass)
  mm <- model.matrix(terms(xreg), xreg)
  if (NROW(mm) == 0 && identical(colnames(mm), "(Intercept)")) {
    return(matrix(data = 1, nrow = NROW(.data), dimnames = list(NULL, "(Intercept)")))
  }
  mm
}