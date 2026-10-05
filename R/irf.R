#' Compute Impulse Response Function (IRF)
#'
#' This function calculates the impulse response function (IRF) of a time series model.
#' The IRF describes how a model's variables react to external shocks over time.
#'
#' If `new_data` contains the `.impulse` column, those values will be
#' treated as impulses for the calculated impulse responses.
#'
#' @param x A fitted model object, such as from a VAR or ARIMA model. This model is used to compute the impulse response.
#' @param ... Additional arguments to be passed to lower-level functions.
#' 
#' @details
#' The impulse response function provides insight into the dynamic behaviour of a system in 
#' response to external shocks. It traces the effect of a one-unit change in the impulse 
#' variable on the response variable over a specified number of periods.
#'
#' For a mable, the number of periods is specified using `h` (or by providing
#' `new_data`). Model specific options are passed via `...`, for example
#' [fable::VAR()] models accept `impulse` (the name of the variable to shock)
#' and `orthogonal` (whether to compute orthogonalised impulse responses).
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibble)
#'
#' # Annual GDP growth and CPI inflation (%) for Australia
#' aus_economy <- tsibbledata::global_economy %>%
#'   dplyr::filter(Country == "Australia") %>%
#'   dplyr::transmute(Growth, Inflation = 100 * difference(log(CPI))) %>%
#'   dplyr::filter(!is.na(Growth), !is.na(Inflation))
#'
#' fit <- aus_economy %>%
#'   model(VAR(vars(Growth, Inflation) ~ AR(1)))
#'
#' # Response of both variables to a unit shock in GDP growth
#' fit %>%
#'   IRF(h = 10, impulse = "Growth")
#'
#' # Orthogonalised response to a shock in inflation
#' fit %>%
#'   IRF(h = 10, impulse = "Inflation", orthogonal = TRUE)
#'
#' @export
IRF <- function(x, ...) {
  UseMethod("IRF")
}

#' @export
IRF.mbl_df <- mbl_df_method(IRF, ".irf", unnest = "tsibble", new_data = "optional")

#' @export
IRF.mdl_df <- mdl_df_method(IRF)

#' @export
IRF.mdl_lst <- mdl_lst_method(IRF, new_data = TRUE)

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