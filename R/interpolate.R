#' Interpolate missing values using a model
#'
#' Fills in the missing values from a model's response variable using the fitted values from the model.
#'
#' @param model a mable
#' @param ... Not used
#'
#' @return
#' A tsibble with missing values interpolated
#'
#' @export
interpolate <- function(model, ...){
  UseMethod("interpolate")
}

#' @export
interpolate.default <- function(model, data){
  resp <- expr_text(model%@%"response")
  missingVals <- is.na(data[[resp]])
  data[[resp]][missingVals] <- fitted(model)[missingVals]
  data
}