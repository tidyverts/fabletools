#' @param bias_adjust Use adjusted back-transformed mean for transformations. 
#' Refer to `vignette("transformations")` for more details.
#' @export
#' @importFrom forecast forecast
#' @importFrom dplyr mutate
forecast.mable <- function(object, new_data = NULL, bias_adjust = TRUE, ...){
  keys <- key(object)
  # Prepare new_data for forecast.model
  object <- bind_new_data(object, new_data)
  
  # Evaluate forecasts
  fc <- map2(object$model, object$new_data, forecast, ...)
  # Modify forecasts with transformations / bias_adjust
  object$fc <- fc <- map2(object$model, fc,
             function(model, fc){
               bt <- invert_transformation((model%@%"fable")$transformation)
               if(isTRUE(bias_adjust)){
                 # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
                 fc[["mean"]] <- bt(fc[["mean"]]) + fc[["sd"]]^2/2*map_dbl(as.numeric(fc[["mean"]]), hessian, func = bt)
               }
               else{
                 fc[["mean"]] <- bt(fc[["mean"]])
               }
               transformation(fc[["distribution"]]) <- bt
               fc[["sd"]] <- NULL
               fc
             })
  
  out <- suppressWarnings(unnest(object, fc, key = keys))
  out$distribution <- fc %>% map(function(x) x[["distribution"]]) %>% invoke(c, .)
  
  as_fable(out, resp = !!sym("mean"), dist = !!sym("distribution"))
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
  fc[["distribution"]] <- dist
  attributes(fc[["distribution"]]) <- attributes(dist)
  as_fable(fc, resp = !!sym("mean"), dist = dist)
}