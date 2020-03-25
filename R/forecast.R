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
#' @param point_forecast Which point forecast measure should be returned in the 
#' resulting fable (possible values include: "mean", "median").
#' @param bias_adjust Deprecated. Please use `point_forecast` to specify the 
#' desired point forecast method.
#' @param ... Additional arguments for forecast model methods.
#' 
#' @return
#' A fable containing the following columns:
#' - `.model`: The name of the model used to obtain the forecast. Taken from
#'   the column names of models in the provided mable.
#' - The point forecast, which by default is the mean. The name of this column
#'   will be the same as the dependent variable in the model(s).
#' - `.distribution`. A column of objects of class `fcdist`, representing the
#'   statistical distribution of the forecast in the given time period.
#' - All columns in `new_data`, excluding those whose names conflict with the
#'   above.
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
forecast.mdl_df <- function(object, new_data = NULL, h = NULL, point_forecast = "mean", ...){
  kv <- c(key_vars(object), ".model")
  mdls <- object%@%"model"
  if(!is.null(h) && !is.null(new_data)){
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  
  # Evaluate forecasts
  object <- dplyr::mutate_at(as_tibble(object), vars(!!!mdls),
                             forecast, object[["new_data"]],
                             h = h, point_forecast = point_forecast, ...,
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
forecast.mdl_ts <- function(object, new_data = NULL, h = NULL, bias_adjust = NULL, point_forecast = "mean", ...){
  if(!is.null(h) && !is.null(new_data)){
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  if(!is.null(bias_adjust)){
    warn("The `bias_adjust` argument for forecast() has been deprecated. Please specify the desired point forecasts using `point_forecast`.\nBias adjusted forecasts are forecast means (`point_forecast = 'mean'`), non-adjusted forecasts are medians (`point_forecast = 'median'`)")
    point_forecast <- if(bias_adjust) "mean" else "median"
  }
  if(is.null(new_data)){
    new_data <- make_future_data(object$data, h)
  }
  if(NROW(new_data) == 0){
    abort("There are no forecasts to be made. Check that your forecast horizon includes at least one future value.")
  }
  
  # Compute specials with new_data
  object$model$stage <- "forecast"
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model),
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
  
  # Back-transform forecast distributions
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
  
  if(length(bt) > 1) abort("Multivariate forecasts are not yet supported")
  
  fc <- bt[[1]](fc)
  
  # Create output object
  idx <- index_var(new_data)
  mv <- measured_vars(new_data)
  resp_vars <- map_chr(object$response, expr_text)
  
  dist_col <- if(length(resp_vars) > 1) ".distribution" else resp_vars
  pred_col <- NULL
  
  new_data[[dist_col]] <- fc
  if ("mean" %in% point_forecast) {
    nm_mean <- if(length(resp_vars) > 1) paste0(".mean_", resp_vars) else ".mean"
    pred_col <- c(pred_col, nm_mean)
    new_data[nm_mean] <- mean(fc)
  }
  if ("median" %in% point_forecast) {
    nm_median <- if(length(resp_vars) > 1) paste0(".median_", resp_vars) else ".median"
    pred_col <- c(pred_col, nm_median)
    new_data[nm_median] <- median(fc)
  }
  cn <- c(dist_col, pred_col)
  
  fbl <- build_tsibble_meta(
    as_tibble(new_data)[unique(c(idx, cn, mv))],
    key_data(new_data),
    index = idx, index2 = idx, ordered = is_ordered(new_data),
    interval = interval(new_data)
  )
  
  as_fable(fbl,
           resp = object$response,
           dist = !!sym(dist_col)
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
