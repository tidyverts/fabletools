#' Evaluate model/forecast accuracy
#' 
#' @param x A model or forecast object
#' @param ... Additional arguments to be passed to other methods
#' 
#' The measures calculated are:
#' \itemize{
#'   \item ME: Mean Error
#'   \item RMSE: Root Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MPE: Mean Percentage Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MASE: Mean Absolute Scaled Error
#'   \item ACF1: Autocorrelation of errors at lag 1.
#' }
#' 
#' @export
accuracy <- function(x, ...){
  UseMethod("accuracy")
}

#' Point estimate accuracy measures
#' 
#' @param .resid A vector of residuals from either the training (model accuracy) or test (forecast accuracy) data.
#' @param .resp A vector of responses matching the residuals (for forecast accuracy, the original data must be provided).
#' @param .period The seasonal period of the data (defaulting to 'smallest' seasonal period).
# #' @param .fitted The fitted values from the model, or forecasted values from the forecast.
# #' @param .dist The distribution of fitted values from the model, or forecasted values from the forecast.
# #' @param .expr_resp An expression for the response variable.
#' @param na.rm Remove the missing values before calculating the accuracy measure
#' @param ... Additional arguments for each measure.
#' @param demean Should the response be demeaned (MASE)
#' @param d Should the response model include a first difference?
#' @param D Should the response model include a seasonal difference?
#' @param na.action Function to handle missing values.
#' 
#' @rdname point-accuracy-measures
#' @export
ME <- function(.resid, na.rm = TRUE, ...){
  mean(.resid, na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
MSE <- function(.resid, na.rm = TRUE, ...){
  mean(.resid ^ 2, na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
RMSE <- function(.resid, na.rm = TRUE, ...){
  sqrt(MSE(.resid, na.rm = na.rm, ...))
}

#' @rdname point-accuracy-measures
#' @export
MAE <- function(.resid, na.rm = TRUE, ...){
  mean(abs(.resid), na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
MPE <- function(.resid, .resp, na.rm = TRUE, ...){
  mean(.resid / .resp * 100, na.rm = TRUE, ...)
}

#' @rdname point-accuracy-measures
#' @export
MAPE <- function(.resid, .resp, na.rm = TRUE, ...){
  mean(abs(.resid / .resp * 100), na.rm = TRUE, ...)
}

#' @rdname point-accuracy-measures
#' @export
MASE <- function(.resid, .resp, demean = FALSE, na.rm = TRUE, .period, d = .period == 1, D = .period > 1, ...){
  if (D > 0) { # seasonal differencing
    y <- diff(.resp, lag = .period, differences = D)
  }
  if (d > 0) {
    y <- diff(y, differences = d)
  }
  if(demean){
    scale <- mean(abs(y - mean(y, na.rm = na.rm, ...)), na.rm = na.rm, ...)
  }
  else{
    scale <- mean(abs(y), na.rm = na.rm, ...)
  }
  mase <- mean(abs(.resid / scale), na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
ACF1 <- function(.resid, na.action = stats::na.pass, ...){
  stats::acf(.resid, plot = FALSE, lag.max = 2, na.action = na.action, ...)$acf[2, 1, 1]
}
