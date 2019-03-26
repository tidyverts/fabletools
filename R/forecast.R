#' Produce forecasts
#' 
#' The forecast function allows you to produce future predictions of a time series
#' from fitted models.
#' 
#' @param object The time series model used to produce the forecasts
#' 
#' @rdname forecast
#' @export
forecast <- function(object, ...){
  UseMethod("forecast")
}

#' @param new_data A `tsibble` containing future information used to forecast.
#' @param h The forecast horison (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param bias_adjust Use adjusted back-transformed mean for transformations. 
#' Refer to `vignette("transformations")` for more details.
#' @param ... Additional arguments for forecast model methods.
#' 
#' @rdname forecast
#' @export
forecast.mdl_df <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
  keys <- c(key(object), sym(".model"))
  mdls <- object%@%"models"
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  object <- gather(object, ".model", ".fit", !!!mdls)
  
  # Evaluate forecasts
  fc <- map2(object$.fit,
             object[["new_data"]] %||% rep(list(NULL), length.out = NROW(object)),
             forecast, h = h, bias_adjust = bias_adjust, ...)
  
  # Construct fable
  out <- add_class(select(as_tibble(object), !!!keys), "lst_ts")
  out <- suppressWarnings(unnest(out, fc, key = keys))
  out[[expr_text(fc[[1]]%@%"dist")]] <- fc %>% map(function(x) x[[expr_text(x%@%"dist")]]) %>% invoke(c, .)
  as_fable(out, resp = !!(fc[[1]]%@%"response"), dist = !!(fc[[1]]%@%"dist"))
}

#' @export
forecast.model <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
  if(is.null(new_data)){
    new_data <- make_future_data(object$index, h)
  }
  
  # Compute specials with new_data
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model)$specials,
                       error = function(e){
                         abort(sprintf(
"%s
Unable to compute required variables from provided `new_data`.
Does your model require extra variables to produce forecasts?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  object$model$remove_data()
  
  
  # Compute forecasts
  fc <- forecast(object$fit, new_data, specials = specials, ...)
  
  # Modify forecasts with transformations / bias_adjust
  bt <- invert_transformation(object$transformation)
  if(isTRUE(bias_adjust)){
    # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
    adjustment <- map_dbl(as.numeric(fc[["point"]]), hessian, func = bt)
    if(any(is.na(adjustment))){
      warning("Could not bias adjust the point forecasts as the back-transformation's hessian is not well behaved. Consider using a different transformation.")
      adjustment <- 0
    }
    fc[["point"]] <- bt(fc[["point"]]) + fc[["sd"]]^2/2*adjustment
  }
  else{
    fc[["point"]] <- bt(fc[["point"]])
  }
  fc[["dist"]] <- update_fcdist(fc[["dist"]], transformation = bt)
  
  out <- mutate(new_data, 
                !!expr_text(object$response) := fc[["point"]],
                .distribution = fc[["dist"]])
  out <- select(out, !!index(out), expr_text(object$response), !!sym(".distribution"), seq_along(out))
  as_fable(out,
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
#' @param point The transformed point forecasts
#' @param sd The standard deviation of the transformed forecasts
#' @param dist The forecast distribution (typically produced using `new_fcdist`)
#' 
#' @export
construct_fc <- function(point, sd, dist){
  stopifnot(is.numeric(point))
  stopifnot(inherits(dist, "fcdist"))
  list(point = point, sd = sd, dist = dist)
}

#' @export
forecast.fbl_ts <- function(object, ...){
  abort("Did you try to forecast a fable? Forecasts can only be computed from model objects (such as a mable).")
}