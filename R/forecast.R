#' Produce forecasts
#' 
#' The forecast function allows you to produce future predictions of a time series
#' from fitted models. If the response variable has been transformed in the
#' model formula, the transformation will be automatically back-transformed
#' (and bias adjusted if `bias_adjust` is `TRUE`). More details about 
#' transformations in the fable framework can be found in
#' `vignette("transformations", package = "fable")`.
#' 
#' The forecasts returned contain both point forecasts and their distribution.
#' A specific forecast interval can be extracted from the distribution using the
#' [`hilo()`] function, and multiple intervals can be obtained using [`report()`].
#' These intervals are stored in a single column using the `hilo` class, to
#' extract the numerical upper and lower bounds you can use [`tidyr::unnest()`].
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
#' Refer to `vignette("transformations", package = "fable")` for more details.
#' @param ... Additional arguments for forecast model methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibble)
#' library(tsibbledata)
#' library(dplyr)
#' library(tidyr)
#' 
#' # Forecasting with an ETS(M,Ad,A) model to Australian beer production
#' beer_fc <- aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   forecast(h = "3 years")
#' 
#' # Compute 80% and 95% forecast intervals
#' beer_fc %>% 
#'   hilo(level = c(80, 95))
#' 
#' beer_fc %>% 
#'   autoplot(aus_production)
#' 
#' # Forecasting with a seasonal naive and linear model to the monthly 
#' # "Food retailing" turnover for each Australian state/territory.
#' library(dplyr)
#' aus_retail %>% 
#'   filter(Industry == "Food retailing") %>% 
#'   model(
#'     snaive = SNAIVE(Turnover),
#'     ets = TSLM(log(Turnover) ~ trend() + season()),
#'   ) %>% 
#'   forecast(h = "2 years 6 months") %>% 
#'   autoplot(filter(aus_retail, Month >= yearmonth("2000 Jan")), level = 90)
#'   
#' # Forecast GDP with a dynamic regression model on log(GDP) using population and
#' # an automatically chosen ARIMA error structure. Assume that population is fixed
#' # in the future.
#' aus_economy <- global_economy %>% 
#'   filter(Country == "Australia")
#' fit <- aus_economy %>% 
#'   model(lm = ARIMA(log(GDP) ~ Population))
#' 
#' future_aus <- new_data(aus_economy, n = 10) %>% 
#'   mutate(Population = last(aus_economy$Population))
#' 
#' fit %>% 
#'   forecast(new_data = future_aus) %>% 
#'   autoplot(aus_economy)
#' }
#' 
#' @rdname forecast
#' @export
forecast.mdl_df <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
  kv <- c(key_vars(object), ".model")
  mdls <- object%@%"models"
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  
  # Evaluate forecasts
  object <- dplyr::mutate_at(as_tibble(object), vars(!!!mdls),
                             forecast, object[["new_data"]],
                             h = h, bias_adjust = bias_adjust, ...,
                             key_data = key_data(object))
  
  object <- gather(object, ".model", ".fc", !!!mdls)
  
  # Combine and re-construct fable
  fbl_attr <- attributes(object$.fc[[1]])
  out <- suppressWarnings(
    unnest_tsbl(as_tibble(object)[c(kv, ".fc")], ".fc", parent_key = kv)
  )
  out[[expr_text(fbl_attr$dist)]] <- invoke(c, map(object$.fc, function(x) x[[expr_text(x%@%"dist")]]))
  as_fable(out, resp = fbl_attr$response, dist = !!fbl_attr$dist)
}

#' @export
forecast.lst_mdl <- function(object, new_data = NULL, key_data, ...){
  map2(object, 
       new_data %||% rep(list(NULL), length.out = length(object)),
       forecast, ...)
}

#' @export
forecast.mdl_ts <- function(object, new_data = NULL, h = NULL, bias_adjust = TRUE, ...){
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
  bt <- map(object$transformation, function(x){
    bt <- invert_transformation(x)
    env <- new_environment(new_data, get_env(bt))
    req_vars <- setdiff(all.vars(body(bt)), names(formals(bt)))
    exists_vars <- map_lgl(req_vars, exists, env)
    if(any(!exists_vars)){
      bt <- custom_error(bt, sprintf(
"Unable to find all required variables to back-transform the forecasts (missing %s).
These required variables can be provided by specifying `new_data`.",
        paste0("`", req_vars[!exists_vars], "`", collapse = ", ")
      ))
    }
    set_env(bt, env)
  })
  
  if(isTRUE(bias_adjust)){
    # Faster version of bias_adjust(bt, fc[["sd"]]^2)(fc[["mean"]]) 
    fc[["point"]] <- pmap(list(fc[["point"]], fc[["sd"]], bt), function(fc, sd, bt) bias_adjust(bt,sd)(fc))
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