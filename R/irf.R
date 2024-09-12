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
#' @export
IRF <- function(x, ...) {
  UseMethod("IRF")
}

#' @export
IRF.mdl_df <- function(x, ...){
  mdl_df_apply(x, IRF, ...)
}

#' @export
IRF.mdl_ts <- function(x, new_data = NULL, h = NULL, ...) {
  if(is.null(new_data)){
    new_data <- make_future_data(x$data, h)
  }
  
  # Compute specials with new_data
  x$model$stage <- "generate"
  x$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(x$model),
                       error = function(e){
                         abort(sprintf(
                           "%s
Unable to compute required variables from provided `new_data`.
Does your model require extra variables to produce simulations?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  x$model$remove_data()
  x$model$stage <- NULL
  
  IRF(x$fit, new_data, specials, ...)
}