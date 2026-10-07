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
#' extract the numerical upper and lower bounds you can use [`unpack_hilo()`].
#' 
#' @param object The time series model used to produce the forecasts
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param h The forecast horison (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param simulate `r lifecycle::badge('deprecated')` Please use [simulate_iid()] to wrap the model instead.
#' @param bootstrap `r lifecycle::badge('deprecated')` Please use [bootstrap_iid()] or [bootstrap_block()] to wrap the model instead.
#' @param times The number of future paths for simulations if `simulate = TRUE`.
#' @param point_forecast The point forecast measure(s) which should be returned 
#' in the resulting fable. Specified as a named list of functions which accept
#' a distribution and return a vector. To compute forecast medians, you can use
#' `list(.median = median)`.
#' @param bias_adjust Deprecated. Please use `point_forecast` to specify the 
#' desired point forecast method.
#' @param ... Additional arguments for forecast model methods.
#' 
#' @return
#' A fable containing the following columns:
#' - `.model`: The name of the model used to obtain the forecast. Taken from
#'   the column names of models in the provided mable.
#' - The forecast distribution. The name of this column will be the same as the 
#'   dependent variable in the model(s). If multiple dependent variables exist,
#'   it will be named `.distribution`.
#' - Point forecasts computed from the distribution using the functions in the
#'   `point_forecast` argument.
#' - All columns in `new_data`, excluding those whose names conflict with the
#'   above.
#'   
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE) && requireNamespace("urca", quietly = TRUE)
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
#' 
#' @rdname forecast
#' @export
forecast.mbl_df <- function(object, new_data = NULL, h = NULL, 
                            point_forecast = list(.mean = mean), ...){
  h <- check_horizon(new_data, h)

  # Deprecated bias adjustment
  if (!is.null(match.call()$bias_adjust)) {
    lifecycle::deprecate_stop("0.2.0", "forecast(bias_adjust = )", "forecast(point_forecast = )")
    point_forecast <- if(match.call()$bias_adjust) list(.mean = mean) else list(.median = stats::median)
  }

  check_reserved_names(object, new_data, key = ".model")
  # The data is bound here, rather than by dispatch_mbl_df(), so that joint
  # forecasting methods (such as reconciliation) get the key structure of all
  # the series to forecast.
  # A mable without any models has no series to bind the data to, so its
  # structure is passed on instead (#313).
  if(!is.null(new_data) && NROW(object) > 0){
    object <- bind_new_data(object, new_data)
    new_data <- NULL
  }
  dispatch_mbl_df(
    object, h = h, point_forecast = point_forecast, ...,
    key_data = key_data(object),
    .f = forecast, .values_to = ".fc", .unnest = unnest_fable,
    .new_data = new_data
  )
}

# Combine the nested forecasts of each model into a fable
unnest_fable <- function(x, col, key) {
  fbl_attr <- attributes(x[[col]][[1]])
  x <- suppressWarnings(unnest_tsbl(x, col, parent_key = key))
  build_fable(x, response = fbl_attr$response, distribution = fbl_attr$dist)
}

#' @export
forecast.mdl_df <- mdl_df_method(forecast)

#' @export
forecast.mdl_lst <- function(object, new_data = NULL, key_data, ...){
  # `key_data` is only used by joint forecasting methods for `mdl_lst`
  # subclasses, and so it isn't passed on to each model.
  dispatch_mdl_lst(object, ..., .f = forecast, .new_data = new_data)
}

#' @export
forecast.lst_mdl <- deprecate_lst_mdl(forecast.mdl_lst)

#' @rdname forecast
#' @export
forecast.mdl_ts <- function(object, new_data = NULL, h = NULL, bias_adjust = NULL,
                            simulate = FALSE, bootstrap = FALSE, times = 5000,
                            point_forecast = list(.mean = mean), ...){
  if(isTRUE(simulate) || isTRUE(bootstrap)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      if(bootstrap) "forecast(bootstrap = )" else "forecast(simulate = )",
      if(bootstrap) "bootstrap_iid()" else "simulate_iid()",
      details = "Wrap the model with `bootstrap_iid()`/`bootstrap_block()`/`simulate_iid()` (via `mutate()`) instead of passing `simulate=`/`bootstrap=` to `forecast()`."
    )
    object <- if(bootstrap) bootstrap_iid(object, times = times) else simulate_iid(object, times = times)
    return(forecast(object, new_data = new_data, h = h, point_forecast = point_forecast, ...))
  }

  setup <- forecast_mdl_ts_setup(object, new_data, h, point_forecast)
  if(!is.null(setup$empty_fbl)) return(setup$empty_fbl)
  new_data <- setup$new_data
  resp_vars <- setup$resp_vars
  dist_col <- setup$dist_col

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
  fc <- forecast(object$fit, new_data, specials = specials, times = times, ...)

  # Back-transform forecast distributions
  bt <- map(object$transformation, function(x){
    trans <- x%@%"inverse"
    inv_trans <- `attributes<-`(x, NULL)
    req_vars <- setdiff(all.vars(body(trans)), names(formals(trans)))
    if(any(req_vars %in% names(new_data))) {
      trans <- lapply(
        vec_chop(new_data[req_vars]),
        function(transform_data) {
          set_env(trans, new_environment(transform_data, get_env(trans)))
        }
      )
      attr(trans, "inverse") <- lapply(
        vec_chop(new_data[req_vars]),
        function(transform_data) {
          set_env(inv_trans, new_environment(transform_data, get_env(inv_trans)))
        }
      )
      trans
    } else {
      structure(list(trans), inverse = list(inv_trans))
    }
  })

  is_transformed <- vapply(bt, function(x) !is_symbol(body(x[[1]])), logical(1L))
  if(length(bt) > 1) {
    if(any(is_transformed)){
      abort("Transformations of multivariate forecasts distributions are not supported, use `simulate_iid()` or `bootstrap_iid()`.")
    }
  }
  if(any(is_transformed)) {
    if (identical(unique(dist_types(fc)), "dist_sample")) {
      fc <- distributional::dist_sample(
        .mapply(exec, list(bt[[1]], distributional::parameters(fc)$x), MoreArgs = NULL)
      )
    } else {
      bt <- bt[[1]]
      fc <- distributional::dist_transformed(fc, `attributes<-`(bt, NULL), bt%@%"inverse")
    }
  }

  forecast_mdl_ts_assemble(new_data, fc, resp_vars, dist_col, point_forecast)
}

#' @rdname forecast
#' @export
forecast.mdl_ts_sim <- function(object, new_data = NULL, h = NULL, times = NULL,
                                point_forecast = list(.mean = mean), ...){
  setup <- forecast_mdl_ts_setup(object, new_data, h, point_forecast)
  if(!is.null(setup$empty_fbl)) return(setup$empty_fbl)
  new_data <- setup$new_data
  resp_vars <- setup$resp_vars
  dist_col <- setup$dist_col

  times <- times %||% (object%@%"times") %||% 5000

  sim <- generate(object, new_data, times = times, ...)
  forecast_from_generate(new_data, sim, resp_vars, dist_col, point_forecast)
}

# Build a forecast fable from generate()'s simulated paths, split by date
# into a dist_sample distribution.
forecast_from_generate <- function(new_data, sim, resp_vars, dist_col, point_forecast) {
  fc_idx <- sim[[index_var(sim)]]
  fc <- if (length(resp_vars) > 1) {
    do.call(cbind, sim[resp_vars])
  } else {
    sim[[".sim"]]
  }
  fc <- distributional::dist_sample(vctrs::vec_split(fc, fc_idx)$val)

  forecast_mdl_ts_assemble(new_data, fc, resp_vars, dist_col, point_forecast)
}

# Shared setup for forecast.mdl_ts()/forecast.mdl_ts_sim(): resolves
# new_data/h and response/distribution column names.
forecast_mdl_ts_setup <- function(object, new_data, h, point_forecast) {
  if(!is.null(h) && !is.null(new_data)){
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }

  if(is.null(new_data)){
    new_data <- make_future_data(object$data, h)
  }
  check_transformation_data(object, new_data)

  # Reseed lag()'s short term memory from this fit's own snapshot.
  object$model$recent_data <- attr(object, "recent_data")

  resp_vars <- vapply(object$response, expr_name, character(1L), USE.NAMES = FALSE)
  dist_col <- if(length(resp_vars) > 1) ".distribution" else resp_vars

  empty_fbl <- NULL
  if(NROW(new_data) == 0){
    empty_fbl <- forecast_mdl_ts_assemble(
      new_data, distributional::new_dist(), resp_vars, dist_col, point_forecast
    )
  }

  list(new_data = new_data, resp_vars = resp_vars, dist_col = dist_col, empty_fbl = empty_fbl)
}

# Shared assembly for forecast.mdl_ts()/forecast.mdl_ts_sim(): attaches
# the forecast distribution and builds the fable.
forecast_mdl_ts_assemble <- function(new_data, fc, resp_vars, dist_col, point_forecast) {
  idx <- index_var(new_data)
  mv <- measured_vars(new_data)

  dimnames(fc) <- resp_vars

  new_data[[dist_col]] <- fc
  point_fc <- compute_point_forecasts(fc, point_forecast)
  new_data[names(point_fc)] <- point_fc

  cn <- c(dist_col, names(point_fc))

  fbl <- build_tsibble_meta(
    as_tibble(new_data)[unique(c(idx, cn, mv))],
    key_data(new_data),
    index = idx, index2 = idx, ordered = is_ordered(new_data),
    interval = interval(new_data)
  )

  build_fable(fbl, response = resp_vars, distribution = dist_col)
}

#' Construct a new set of forecasts
#' 
#' @description 
#' `r lifecycle::badge('deprecated')`
#' 
#' This function is deprecated. `forecast()` methods for a model should return
#' a vector of distributions using the distributional package.
#' 
#' Backtransformations are automatically handled, and so no transformations should be specified here.
#' 
#' @param point The transformed point forecasts
#' @param sd The standard deviation of the transformed forecasts
#' @param dist The forecast distribution (typically produced using `new_fcdist`)
#' 
#' @keywords internal
#' @export
construct_fc <- function(point, sd, dist){
  lifecycle::deprecate_stop("0.3.0", what = "fabletools::construct_fc()",
                            details = "The forecast function should now return a vector of distributions from the 'distributional' package.")
}

compute_point_forecasts <- function(distribution, measures){
  map(measures, function(f){
    out <- f(distribution)
    # Empty distributions give NULL rather than an empty point forecast (#313)
    if(is.null(out) && length(distribution) == 0){
      resp <- dimnames(distribution)
      out <- if(length(resp) > 1) matrix(numeric(), 0, length(resp), dimnames = list(NULL, resp)) else numeric()
    }
    out
  })
}

#' @export
forecast.fbl_ts <- function(object, ...){
  abort("Did you try to forecast a fable? Forecasts can only be computed from model objects (such as a mable).")
}

#' A set of future scenarios for forecasting
#' 
#' @param ... Input data for each scenario
#' @param names_to The column name used to identify each scenario
#' 
#' @export
scenarios <- function(..., names_to = ".scenario"){
  structure(list2(
    ...
  ), names_to = names_to)
}