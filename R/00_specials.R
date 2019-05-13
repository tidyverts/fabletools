#' Create evaluation environment for specials
#' 
#' Allows extension packages to make use of the formula parsing of specials.
#' 
#' @param ... A named set of functions which used to parse formula inputs
#' @param .required_specials The names of specials which must be provided (and if not, are included with no inputs).
#' 
#' @export
new_specials <- function(..., .required_specials = NULL){
  specials <- squash(list2(...))
  if(is.null(specials$xreg)){
    specials$xreg <- function(...) abort(sprintf("Exogenous regressors is not supported for %s.", self$model))
  }
  structure(specials,
            required_specials = .required_specials,
            class="fable_specials")
}

#' Bind values to the functions of a specials environment
#' 
#' @param .specials The specials environment
#' @param ... Values to be bound to the functions of the specials environment
#' 
#' @export
specials_fn_bind <- function(.specials, ...){
  imap(as.list(.specials), function(fn, nm){
    assign(nm,
           set_env(fn, env_bury(get_env(fn), ...)),
           envir = .specials
    )
  })
}