#' Forecast a mable
#' 
#' @param object A mable containing models used for forecasting.
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param h The forecast horison (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param bias_adjust Use adjusted back-transformed mean for transformations. 
#' Refer to `vignette("transformations")` for more details.
#' @param ... Further arguments to forecast model methods.
#' 
#' @export
#' @importFrom forecast forecast
#' @importFrom dplyr mutate
forecast.mdl_df <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
  # Prepare new_data for forecast.model
  if(is.null(new_data)){
    lst_fits <- nest(group_by_key(fitted(object)))
    if(is.null(h)){
      h <- map_dbl(lst_fits$data, function(.x) get_frequencies("smallest", .x)*2)
    }
    lst_fits[["new_data"]] <- map2(lst_fits$data, h,
                     function(data, h){
                       idx <- expr_text(index(data))
                       future <- seq(data[[idx]][[NROW(data)]], length.out = h + 1, by = time_unit(interval(data)))[-1]
                       build_tsibble(list2(!!idx := future), key = id(), index = idx)
                     })
    new_data <- unnest(lst_fits, new_data, key = key(object))
  }
  
  object <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(object)
  object <- bind_new_data(object, new_data)
  
  # Evaluate forecasts
  fc <- map2(object$.fit, object$new_data, forecast, bias_adjust = bias_adjust, ...)
  
  out <- suppressWarnings(unnest(add_class(object, "lst_ts"), fc, key = keys))
  out[[expr_text(fc[[1]]%@%"dist")]] <- fc %>% map(function(x) x[[expr_text(x%@%"dist")]]) %>% invoke(c, .)
  
  as_fable(out, resp = !!response(fc[[1]]), dist = !!(fc[[1]]%@%"dist"))
}

#' @export
forecast.model <- function(object, new_data, bias_adjust = TRUE, ...){
  fc <- forecast(object$fit, new_data, ...)
  
  # Modify forecasts with transformations / bias_adjust
  bt <- invert_transformation(object$transformation)
  if(isTRUE(bias_adjust)){
    # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
    fc[["point"]] <- bt(fc[["point"]]) +
      fc[["sd"]]^2/2*map_dbl(as.numeric(fc[["point"]]), hessian, func = bt)
  }
  else{
    fc[["point"]] <- bt(fc[["point"]])
  }
  transformation(fc[["dist"]]) <- bt
  
  as_fable(transmute(new_data, 
                     !!as_string(object$response) := fc[["point"]],
                     .distribution = fc[["dist"]]),
           resp = !!object$response,
           dist = !!sym(".distribution")
  )
}

#' Construct a new set of forecasts
#' 
#' Will be deprecated in the future, forecast objects should be produced with
#' either `fable` or `as_fable` functions.
#' 
#' Backtransformations are automatically handled, and so no transformations should be specified here.
#' 
#' @param newdata The newdata provided to the forecast function
#' @param point The transformed point forecasts
#' @param sd The standard deviation of the transformed forecasts
#' @param dist The forecast distribution (typically produced using `new_fcdist`)
#' @param response The column name of the response variable
#' 
#' @export
construct_fc <- function(point, sd, dist){
  stopifnot(is.numeric(point))
  stopifnot(inherits(dist, "fcdist"))
  list(point = point, sd = sd, dist = dist)
}