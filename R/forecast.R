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
  kv <- c(key_vars(object), ".model")
  mdls <- object%@%"models"
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  
  fc <- dplyr::mutate_at(as_tibble(object),
                         vars(!!!mdls), forecast, object[["new_data"]],
                         h = h, bias_adjust = bias_adjust, ...,
                         key_data = key_data(object))
  
  fc_interval <- interval(fc[[mdls[[1]]]][[1]])
  fc_idx <- index(fc[[mdls[[1]]]][[1]])
  # Combine and re-construct fable
  fc <- gather(fc, ".model", ".fc", !!!mdls)

  idx <- index(fc[[".fc"]][[1]])
  resp <- fc[[".fc"]][[1]]%@%"response"
  dist <- fc[[".fc"]][[1]]%@%"dist"
  
  dist_repaired <- fc[[".fc"]] %>%
    map(function(x) x[[expr_text(x%@%"dist")]]) %>%
    invoke(c, .)
  
  fc <- suppressWarnings(unnest(fc, !!sym(".fc")))
  fc[[expr_text(dist)]] <- dist_repaired
  fc <- build_tsibble(fc, key = kv, index = !!fc_idx, interval = fc_interval)
  
  as_fable(fc, index = !!idx, key = kv, resp = resp, dist = !!dist)
}

#' @export
forecast.lst_mdl <- function(object, new_data = NULL, key_data, ...){
  map2(object, 
       new_data %||% rep(list(NULL), length.out = length(object)),
       forecast, ...)
}

#' @export
forecast.model <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
  if(is.null(new_data)){
    new_data <- make_future_data(object$data, h)
  }
  
  # Compute specials with new_data
  object$model$stage <- "forecast"
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
  object$model$stage <- NULL

  # Compute forecasts
  fc <- forecast(object$fit, new_data, specials = specials, ...)
  
  # Modify forecasts with transformations / bias_adjust
  bt <- map(object$transformation, invert_transformation)
  if(isTRUE(bias_adjust)){
    # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
    adjustment <- map2(fc[["point"]], bt, function(fc, bt) map_dbl(fc, hessian, func = bt))

    fc[["point"]] <- map2(fc[["point"]], bt, function(fc, bt) bt(fc))
    if(any(map2_lgl(fc[["point"]], adjustment, function(fc, adj) any(!is.na(fc) & is.na(adj))))){
      warn("Could not bias adjust the point forecasts as the back-transformation's hessian is not well behaved. Consider using a different transformation.")
    }
    else if(any(map_lgl(fc[["sd"]], compose(any, is.na)))){
      warn("Could not bias adjust the point forecasts as the forecast standard deviation is unknown. Perhaps your series is too short or insufficient bootstrap samples are used.")
    }
    else{
      adjustment <- map2(fc[["sd"]], adjustment, function(sd, adj) sd^2/2*adj)
      fc[["point"]] <- map2(fc[["point"]], adjustment, "+")
    }
  }
  else{
    fc[["point"]] <- map2(fc[["point"]], bt, function(fc, bt) bt(fc))
  }
  
  fc[["dist"]] <- update_fcdist(fc[["dist"]], transformation = bt)
  fc[["point"]] <- set_names(fc[["point"]], map_chr(object$response, expr_text))
  
  out <- mutate(new_data, 
                !!!(fc[["point"]]),
                .distribution = fc[["dist"]])
  out <- select(out, !!index(out), names(fc[["point"]]), !!sym(".distribution"), seq_along(out))
  as_fable(out,
           resp = object$response,
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
  stopifnot(inherits(dist, "fcdist"))
  if(is.numeric(point)){
    point <- list(point)
    sd <- list(sd)
  }
  list(point = point, sd = sd, dist = dist)
}

#' @export
forecast.fbl_ts <- function(object, ...){
  abort("Did you try to forecast a fable? Forecasts can only be computed from model objects (such as a mable).")
}