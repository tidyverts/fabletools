#' Extract the left hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_lhs <- function(model){
  if(is_formula(model)){
    transform_spec <- f_lhs(model)
  }
  else{
    transform_spec <- model
  }
}

#' Extract the right hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_rhs <- function(model){
  if(is_formula(model)){
    f_rhs(model)
  }
  else{
    expr(NULL)
  }
}

#' @importFrom stats formula
#' @export
formula.fable_model <- function(x, ...){
  (x%@%"fable")$model
}

#' @export
transformation.fable_model <- function(x, ...){
  (x%@%"fable")$transformation
}

#' Extract the response variable from an object
#' 
#' @param x An object containing a response variable (such as a `fable_model`)
#' 
#' @return An expression for the response variable
#' @export
response <- function(x, ...){
  UseMethod("response")
}

#' @export
response.fable_model <- function(x, ...){
  (x%@%"fable")$response
}