#' Identify outliers
#' 
#' Return a table of outlying observations using a fitted model.
#'
#' `outliers()` is a generic for identifying unusual observations in the data
#' used to estimate a model. For a mable, each model's method is used to
#' identify which observations are outliers, and the corresponding rows of the
#' model's training data are returned as a tsibble (with a `.model` column
#' identifying the model that detected them).
#'
#' @details
#' This functionality is experimental, and currently very few models provide
#' an `outliers()` method (one example is the X-13ARIMA-SEATS decomposition
#' from the feasts package). Model packages can support outlier detection by
#' implementing an `outliers()` method for their model class, which should
#' return the positions (or a logical vector) of the outlying observations.
#'
#' A default model-based approach to outlier detection is planned, which will
#' identify observations that are unusual relative to the model's one-step
#' ahead fitted values (for example, residuals outside of an interquartile
#' range based threshold). Outlier detection is intended to be one part of a
#' broader workflow for cleaning time series, where outliers are identified
#' with `outliers()`, replaced with missing values, and then filled in using
#' [`interpolate()`]. A higher level function combining these steps may be
#' added once suitable defaults are better understood.
#'
#' @param object An object which can identify outliers.
#' @param ... Arguments for further methods.
#' 
#' @rdname outliers
#' @export
outliers <- function(object, ...){
  UseMethod("outliers")
}

#' @rdname outliers
#' @export
outliers.mbl_df <- function(object, ...){
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  object <- mutate(as_tibble(object), 
                   dplyr::across(all_of(mbl_vars), function(x) lapply(x, outliers, ...)))
  object <- pivot_longer(object, all_of(mbl_vars), names_to = ".model", values_to = ".outliers")
  unnest_tsbl(object, ".outliers", parent_key = c(kv, ".model"))
}

#' @rdname outliers
#' @export
outliers.mdl_ts <- function(object, ...){
  object$data[outliers(object$fit, ...),]
}