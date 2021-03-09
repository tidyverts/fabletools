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
#' @param default_intercept Should an intercept be included if not specified in the model?
#' 
#' @importFrom stats model.frame
#' @export
special_xreg <- function(default_intercept = TRUE) {
  out <- function(...) {
    default_intercept <- !!default_intercept
    dots <- enexprs(...)
    constants <- map_lgl(dots, inherits, "numeric")
    constant_specified <- any(map_lgl(dots[constants], `%in%`, c(-1, 0, 1)))
    if(!default_intercept && !constant_specified) {
      dots <- c(dots, list(0))
    }
    
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(dots, function(.x, .y) call2("+", .x, .y))
    )
    
    env <- map(enquos(...), get_env)
    env[map_lgl(env, compose(is_empty, env_parents))] <- NULL
    env <- if (!is_empty(env)) get_env(env[[1]]) else base_env()
    
    xreg <- model.frame(model_formula, data = env, na.action = stats::na.pass)
    mm <- model.matrix(terms(xreg), xreg)
    if (NROW(mm) == 0 && identical(colnames(mm), "(Intercept)")) {
      return(matrix(data = 1, nrow = NROW(self$data), dimnames = list(NULL, "(Intercept)")))
    }
    mm
  }
  body(out)[[2]] <- expr(default_intercept <- !!default_intercept)
  out
}