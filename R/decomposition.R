globalVariables("self")

train_decomposition <- function(.data, dcmp_fn, dcmp_args, formula,
                                specials, ...){
  # Extract raw original data
  est <- .data
  .data <- self$data
  
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

decomposition_model <- R6::R6Class("decomposition",
                                   inherit = model_definition,
                                   public = list(
                                     model = "decomposition",
                                     train = train_decomposition,
                                     specials = NULL,
                                     dcmp_fn = NULL,
                                     dcmp_args = NULL,
                                     initialize = function(.f, formula, ..., .f_args){
                                       self$dcmp_fn <- .f
                                       self$dcmp_args <- .f_args
                                       self$formula <- enquo(formula)
                                       self$extra <- list2(...)
                                     }
                                   )
)

#' Decomposition modelling
#' 
#' @param .f The decomposition function (such as `STL`)
#' @param formula The formula used to describe the decomposition
#' @param ... Model definitions used to model the components (such as `ETS`)
#' @param .f_args Arguments to be passed to the decomposition function (`.f`)
#' 
#' @export
decomposition <- decomposition_model$new

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