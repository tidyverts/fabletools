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
  keys <- key(object)
  
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
    new_data <- unnest(lst_fits, new_data, key = keys)
  }
  object <- bind_new_data(object, new_data)
  
  # Evaluate forecasts
  fc <- map2(object$model, object$new_data, forecast, ...)
  # Modify forecasts with transformations / bias_adjust
  object$fc <- fc <- map2(object$model, fc,
             function(model, fc){
               bt <- invert_transformation((model%@%"fable")$transformation)
               if(isTRUE(bias_adjust)){
                 # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
                 fc[[expr_text(response(fc))]] <- bt(fc[[expr_text(response(fc))]]) +
                   fc[["sd"]]^2/2*map_dbl(as.numeric(fc[[expr_text(response(fc))]]), hessian, func = bt)
               }
               else{
                 fc[[expr_text(response(fc))]] <- bt(fc[[expr_text(response(fc))]])
               }
               transformation(fc[[expr_text(fc%@%"dist")]]) <- bt
               fc[["sd"]] <- NULL
               fc
             })
  
  out <- suppressWarnings(unnest(add_class(object, "lst_ts"), fc, key = keys))
  out[[expr_text(fc[[1]]%@%"dist")]] <- fc %>% map(function(x) x[[expr_text(x%@%"dist")]]) %>% invoke(c, .)
  
  as_fable(out, resp = !!response(fc[[1]]), dist = !!(fc[[1]]%@%"dist"))
}

#' Construct a new set of forecasts
#' 
#' Allows extension packages to create a tsibble forecast object.
#' This structure is a suitable output for model extension package's forecast generic.
#' 
#' Backtransformations are automatically handled, and so no transformations should be specified here.
#' 
#' @param newdata The newdata provided to the forecast function
#' @param point The point transformed forecasts
#' @param sd The standard deviation of the transformed forecasts
#' @param dist The forecast distribution (typically produced using `new_fcdist`)
#' 
#' @export
construct_fc <- function(newdata, point, sd, dist){
  stopifnot(is_tsibble(newdata))
  stopifnot(is.numeric(point))
  stopifnot(inherits(dist, "fcdist"))
  fc <- newdata[expr_text(index(newdata))]
  fc[["mean"]] <- point
  fc[["sd"]] <- sd
  fc[[".distribution"]] <- dist
  attributes(fc[[".distribution"]]) <- attributes(dist)
  as_fable(fc, resp = !!sym("mean"), dist = !!sym(".distribution"))
}