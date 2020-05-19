#' Return response variables
#' 
#' `response_vars()` returns a character vector of the response variables in the
#' object.
#' 
#' @param x A dataset containing a response variable (such as a mable, fable, or dable).
#' @export
response_vars <- function(x){
  UseMethod("response_vars")
}

#' @export
response_vars.fbl_ts <- function(x){
  x%@%"response"
}
#' @export
response_vars.mdl_df <- function(x){
  x%@%"response"
}
#' @export
response_vars.dcmp_ts <- function(x){
  x%@%"response"
}

#' Return distribution variable
#' 
#' `distribution_var()` returns a character vector of the distribution variable 
#' in the data.
#' 
#' @param x A dataset containing a distribution variable (such as a fable).
#' @export
distribution_var <- function(x){
  UseMethod("distribution_var")
}
#' @export
distribution_var.fbl_ts <- function(x){
  x%@%"dist"
}

#' Return model column variables
#' 
#' `mable_vars()` returns a character vector of the model variables in the
#' object.
#' 
#' @param x A dataset containing models (such as a mable).
#' @export
mable_vars <- function(x){
  UseMethod("mable_vars")
}
#' @export
mable_vars.mdl_df <- function(x){
  x%@%"model"
}