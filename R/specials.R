#' Create evaluation environment for specials
#' 
#' Allows extension packages to make use of the formula parsing of specials.
#' 
#' @param ... A named set of functions which used to parse formula inputs
#' @param .env The evaluation environment of the specials (to find other user objects)
#' @param .required_specials The names of specials which must be provided (and if not, are included used with no inputs).
#' @param .bury If TRUE, the specials are bound to a child environment of `.env`.
#' @param .vals A list of named values to be bound to the special functions
#' 
#' @export
new_specials_env <- function(..., .env = caller_env(), .required_specials = NULL, .bury = TRUE, .vals = NULL){
  if(.bury){
    .env <- child_env(.env)
  }
  else{
    .env <- .env
  }
  
  env_bind(.env,
    !!!map(dots_list(...),
      function(fn){
        set_env(fn, env_bury(get_env(fn), !!!.vals))
      }
    )
  )
  
  structure(.env, required_specials = .required_specials)
}