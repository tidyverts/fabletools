new_model <- function(fit = NULL, model, data, response, transformation,
                       recent_data = NULL){
  # Dispatch on `model` (the mdl_spec), not `fit`, so that a modifier applied
  # to a model specification (bootstrap_iid()/bootstrap_block()/
  # simulate_iid() on an unfitted `mdl_spec`) can wrap the resulting mdl_ts
  # by providing a new_model() method for the class it adds to the spec.
  UseMethod("new_model", model)
}

new_model.default <- function(fit = NULL, model, data, response, transformation,
                               recent_data = NULL){
  structure(list(fit = fit, model = model, data = data,
                 response = response, transformation = transformation),
            recent_data = recent_data,
            class = "mdl_ts")
}

# Restore a fit's lag() short term memory and trend() origin to its model
# definition, which is shared by the fits of every series.
restore_model_state <- function(object){
  object$model$recent_data <- attr(object, "recent_data")
  origin <- attr(object, "origin")
  if(!is.null(origin)) object$model$origin <- origin
  invisible(object)
}

#' @export
format.mdl_ts <-  function(x, ...){
  model_sum(x)
}

type_sum.mdl_ts <-  function(x){
  model_sum(x[["fit"]])
}

#' Is the object a model
#' 
#' @param x An object.
#' 
#' @export
is_model <- function(x){
  inherits(x, "mdl_ts")
}

#' Provide a succinct summary of a model
#' 
#' Similarly to pillar's type_sum and obj_sum, model_sum is used to provide brief model summaries.
#' 
#' @param x The model to summarise
#' 
#' @export
model_sum <- function(x){
  UseMethod("model_sum")
}

#' @export
model_sum.default <- function(x){
  tibble::type_sum(x)
}

#' @export
model_sum.mdl_ts <-  function(x){
  model_sum(x[["fit"]])
}