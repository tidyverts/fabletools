#' @importFrom utils tail
fc_idx <- function(idx, h){
  seq(tail(idx, 1), length.out = h + 1, by = time_unit(idx)) %>% tail(-1)
}

#' @export
#' @importFrom forecast forecast
#' @importFrom dplyr mutate
forecast.mable <- function(object, h = NULL, newdata = NULL, biasadj = TRUE, bootstrap = FALSE, ...){
  if(bootstrap){
    abort("bootstrapping is not yet supported")
  }
  
  # Prepare newdata for forecast.model
  if(is.null(newdata)){
    if(is.null(h)){
      h <- map_dbl(object$data, ~ get_frequencies("smallest", .x)*2)
    }
    object[["newdata"]] <- map2(object$data, h,
                                function(data, h){
                                  idx <- expr_text(index(data))
                                  future <- fc_idx(data[[idx]], h)
                                  build_tsibble(list2(!!idx := future), key = id(), index = idx)
                                })
  }
  else{
    newdata <- newdata %>% 
      group_by(!!!syms(key_vars(object))) %>% 
      nest(.key = "newdata")
    if(length(key_vars(object)) > 0){
      object <- left_join(object, newdata, by = key_vars(object))
    }
    else{
      object[["newdata"]] <- newdata[["newdata"]]
    }
  }
  # Evaluate forecasts
  fc <- map2(object$model, object$newdata, forecast, ...)
  # Modify forecasts with transformations / biasadj
  fc <- map2(object$model, fc,
             function(model, fc){
               bt <- invert_transformation((model%@%"fable")$transformation)
               if(isTRUE(biasadj)){
                 fc[["mean"]] <- biasadj(bt, fc[["se"]]^2)(fc[["mean"]])
               }
               else{
                 fc[["mean"]] <- bt(fc[["mean"]])
               }
               transformation(fc[["distribution"]]) <- bt
               fc
             })
  
  fc[["se"]] <- NULL
  
  fable(object, fc)
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
#' @param se The standard error of the transformed forecasts
#' @param dist The forecast distribution (typically produced using `new_fcdist`)
#' 
#' @export
construct_fc <- function(newdata, point, se, dist){
  stopifnot(is_tsibble(newdata))
  stopifnot(is.numeric(point))
  stopifnot(inherits(dist, "fcdist"))
  fc <- newdata[expr_text(index(newdata))]
  fc[["mean"]] <- point
  fc[["se"]] <- se
  fc[["distribution"]] <- dist
  attributes(fc[["distribution"]]) <- attributes(dist)
  new_fc(fc)
}

new_fc <- function(x){
  stopifnot(is_tsibble(x))
  add_class(x, "tbl_fc")
}