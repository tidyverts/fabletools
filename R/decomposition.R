train_decomposition <- function(.data, dcmp_fn, dcmp_args, formula,
                                specials, ...){
  # Extract raw original data
  est <- .data
  .data <- specials[["xreg"]][[1]]
  
  dcmp <- do.call(dcmp_fn, list2(.data, formula, !!!dcmp_args))
  dcmp_method <- dcmp%@%"dcmp"
  req_vars <- all.vars(dcmp_method)
  
  mdls <- model(dcmp, ...)
  
  mdl_cols <- map_chr(mdls%@%"models", as_string)
  
  available_vars <- map_chr(mdl_cols, function(cn){
    all.vars(mdls[[cn]][[1]]$response)
  })
  
  structure(
    list(
      est = est %>% 
        mutate(
          #.fitted = fitted,
          #.resid = residuals
        ),
      fit = tibble(method = "Decomposition model",
                   decomposition = list(dcmp_method)),
      models = mdls
    ),
    class = "decomposition_model"
  )
}

specials_decomposition <- new_specials(
  xreg = function(...){
    .data
  },
  .required_specials = "xreg"
)

#' Decomposition modelling
#' 
#' @param .f The decomposition function (such as `STL`)
#' @param formula The formula used to describe the decomposition
#' @param ... Model definitions used to model the components (such as `ETS`)
#' @param .f_args Arguments to be passed to the decomposition function (`.f`)
#' 
#' @export
decomposition <- function(.f, formula, ..., .f_args = list()){
  structure(
    list(
      formula = enquo(formula),
      train = train_decomposition, 
      specials = specials_decomposition, 
      dots = list(dcmp_fn = .f, ..., dcmp_args = .f_args)),
    class = "model_definition"
  )
}

#' @export
fitted.decomposition_model <- function(object, ...){
  select(object$est, ".fitted")
}

#' @export
residuals.decomposition_model <- function(object, ...){
  select(object$est, ".resid")
}

#' @export
augment.decomposition_model <- function(x, ...){
  x$est
}

#' @export
glance.decomposition_model <- function(x, ...){
  x$fit
}

#' @export
tidy.decomposition_model <- function(x, ...){
  x$par
}

#' @export
model_sum.decomposition_model <- function(x){
  x$fit$method
}