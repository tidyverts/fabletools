#' Compute Impulse Response Function (IRF)
#'
#' This function calculates the impulse response function (IRF) of a time series model.
#' The IRF describes how a model's variables react to external shocks over time.
#'
#' @param x A fitted model object, such as from a VAR or ARIMA model. This model is used to compute the impulse response.
#' @param impulse A character string specifying the name of the variable that is shocked (the impulse variable).
#' @param ... Additional arguments to be passed to lower-level functions.
#' 
#' @details
#' The impulse response function provides insight into the dynamic behaviour of a system in 
#' response to external shocks. It traces the effect of a one-unit change in the impulse 
#' variable on the response variable over a specified number of periods.
#'
IRF <- function(x, ...) {
  UseMethod("IRF")
}

#' @export
IRF.mdl_df <- function(x, ...){
  mdl_df_apply(x, IRF)
}

#' @export
IRF.mdl_ts <- function(x, ...) {
  IRF(x$fit, ...)
}