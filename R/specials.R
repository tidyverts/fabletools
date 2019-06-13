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
