new_model <- function(fit = NULL, model, data, response, transformation){
  structure(list(fit = fit, model = model, data = data,
                 response = response, transformation = transformation),
            class = "mdl_ts")
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