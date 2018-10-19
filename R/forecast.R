#' @export
#' @importFrom forecast forecast
#' @importFrom dplyr mutate
forecast.mable <- function(object, new_data = NULL, biasadj = TRUE, ...){
  keys <- key(object)
  # Prepare new_data for forecast.model
  object <- bind_new_data(object, new_data)
  
  # Evaluate forecasts
  fc <- map2(object$model, object$new_data, forecast, ...)
  # Modify forecasts with transformations / biasadj
  object$fc <- fc <- map2(object$model, fc,
             function(model, fc){
               bt <- invert_transformation((model%@%"fable")$transformation)
               if(isTRUE(biasadj)){
                 # Faster version of biasadj(bt, fc[["sd"]]^2)(fc[["mean"]]) 
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
  add_class(out, "fbl_ts")
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
  new_fc(fc)
}

new_fc <- function(x){
  stopifnot(is_tsibble(x))
  add_class(x, "tbl_fc")
}