#' Produce forecasts
#' 
#' The forecast function allows you to produce future predictions of a time series
#' from fitted models. If the response variable has been transformed in the
#' model formula, the forecast distribution will be automatically
#' back-transformed, and the point forecasts are computed from it. More details
#' about transformations in the fable framework can be found in
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
#' @param h The forecast horizon (can be used instead of `new_data` for regular
#' time series with no exogenous regressors). This can be a number of steps, or
#' text such as `"1 year"` for the number of steps covering that calendar
#' period after the end of the data.
#' @param simulate `r lifecycle::badge('deprecated')` Please use [simulate_iid()] to wrap the model instead.
#' @param bootstrap `r lifecycle::badge('deprecated')` Please use [bootstrap_iid()] or [bootstrap_block()] to wrap the model instead.
#' @param times `r lifecycle::badge('deprecated')` The number of sample paths
#' for the deprecated `simulate` and `bootstrap` arguments. Please use the
#' `times` argument of [simulate_iid()], [bootstrap_iid()] or
#' [bootstrap_block()] instead, which this doesn't change.
#' @param point_forecast The point forecast measure(s) which should be returned 
#' in the resulting fable. Specified as a named list of functions which accept
#' a distribution and return a vector. To compute forecast medians, you can use
#' `list(.median = median)`.
#' @param ... Additional arguments for forecast model methods.
#' 
#' @return
#' For a model, its forecast distribution with one value for each future time
#' point. A list of models (a column of a mable) gives a list of the forecast
#' distributions of each model. Point forecasts are only computed for a mable.
#'
#' For a mable, a fable containing the following columns:
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
  check_reserved_names(object, new_data, key = ".model")
  dots <- list2(...)
  check_removed_bias_adjust(dots)
  object <- modify_each_model(object, deprecated_sim_modifier(dots, "forecast", user_env = caller_env()))
  if (!is.null(dots$times)) {
    if (!isTRUE(dots$simulate) && !isTRUE(dots$bootstrap)) {
      lifecycle::deprecate_warn(
        "1.0.0", "forecast(times = )",
        details = "Set the number of sample paths with the `times` argument of `simulate_iid()`, `bootstrap_iid()` or `bootstrap_block()`, which `times` doesn't change.",
        user_env = caller_env()
      )
    } else {
      dots$times <- NULL
    }
  }
  dots[c("simulate", "bootstrap", "bootstrap_block_size")] <- NULL

  # The data is bound here, rather than by dispatch_mbl_df(), so that joint
  # forecasting methods (such as reconciliation) get the key structure of all
  # the series to forecast. The future time points for a horizon `h` are also
  # bound to each row, as the forecast distributions from the models are
  # combined with them into a fable.
  # A mable without any models has no series to bind the data to, so its
  # structure is passed on instead (#313).
  if(NROW(object) > 0){
    object <- if(is.null(new_data)) bind_future_data(object, h) else bind_new_data(object, new_data)
    new_data <- NULL
  } else if(is.null(new_data) && !is.null(ptype <- mable_ptype(object))){
    new_data <- make_future_data(ptype$data, h)
  }
  response <- unname(response_vars(object))
  # The point forecasts are computed from the distributions here, so methods
  # still computing them for each model (and returning a fable) compute none.
  inject(dispatch_mbl_df(
    object, point_forecast = list(), !!!dots,
    key_data = key_data(object),
    .f = forecast, .values_to = ".fc", .keep_new_data = TRUE,
    .unnest = function(x, col, key) unnest_fable(x, col, key, response, point_forecast),
    .new_data = new_data
  ))
}

# Bind the future time points of each series for horizon `h` as its
# `new_data`, from the data of its first model with any.
bind_future_data <- function(object, h) {
  mdls <- as_tibble(object)[mable_vars(object)]
  ptype <- mable_ptype(object)
  object[["new_data"]] <- lapply(seq_len(NROW(object)), function(i) {
    data <- NULL
    for (mdl in row_models(mdls, i)) {
      if (NROW(mdl$data) > 0) {
        data <- mdl$data
        break
      }
    }
    make_future_data(data %||% ptype$data, h)
  })
  object
}

# The models of row `i` of a mable's model columns
row_models <- function(x, i) {
  unlist(lapply(unclass(x), function(col) {
    if (is.data.frame(col)) row_models(col, i) else list(col[[i]])
  }), recursive = FALSE)
}

# The modifier replacing the deprecated `simulate`/`bootstrap` arguments of
# forecast() and generate() (with a deprecation warning), or NULL if unused.
deprecated_sim_modifier <- function(dots, fn, user_env = caller_env(2), call = caller_env()) {
  for (arg in intersect(c("simulate", "bootstrap"), names(dots))) {
    if (!is_bool(dots[[arg]])) {
      abort(sprintf("`%s` must be `TRUE` or `FALSE`.", arg), call = call)
    }
  }
  simulate <- isTRUE(dots$simulate) && fn == "forecast"
  bootstrap <- isTRUE(dots$bootstrap)
  if (!simulate && !bootstrap) return(NULL)
  block_size <- dots$bootstrap_block_size %||% 1
  lifecycle::deprecate_warn(
    "1.0.0",
    sprintf("%s(%s = )", fn, if (bootstrap) "bootstrap" else "simulate"),
    if (!bootstrap) "simulate_iid()" else if (block_size > 1) "bootstrap_block()" else "bootstrap_iid()",
    details = sprintf(
      "Modify the model within `mutate()` instead, e.g. `mutate(fit, ets = %s(ets))`.",
      if (!bootstrap) "simulate_iid" else if (block_size > 1) "bootstrap_block" else "bootstrap_iid"
    ),
    user_env = user_env
  )
  times <- if (fn == "forecast") dots$times %||% 5000 else 5000
  if (!bootstrap) {
    function(x) simulate_iid(x, times = times)
  } else if (block_size > 1) {
    function(x) bootstrap_block(x, times = times, block_size = block_size)
  } else {
    function(x) bootstrap_iid(x, times = times)
  }
}

# Apply the modifier replacing deprecated arguments to each model of a mable,
# independently as the deprecated arguments did.
modify_each_model <- function(object, modifier) {
  if (is.null(modifier)) return(object)
  mdls <- mable_vars(object)
  dplyr::dplyr_col_modify(object, lapply(as_tibble(object)[mdls], map_models, modifier))
}

# Combine the forecast distributions of each model with the data of their
# future time points, and compute their point forecasts, as a fable.
unnest_fable <- function(x, col, key, response, point_forecast) {
  dist_col <- if(length(response) > 1) ".distribution" else response
  x[[col]] <- map2(x[["new_data"]], x[[col]], function(new_data, fc) {
    fc <- as_forecast_distribution(fc)
    if(vec_size(fc) != NROW(new_data)) {
      abort(sprintf(
        "The forecast distribution of a model has %d values, but %d future time points were forecast.",
        vec_size(fc), NROW(new_data)
      ), call = NULL)
    }
    dist_variates(fc) <- response
    new_data[[dist_col]] <- fc
    new_data[unique(c(index_var(new_data), dist_col, names(new_data)))]
  })
  x[["new_data"]] <- NULL
  x <- suppressWarnings(unnest_tsbl(x, col, parent_key = key))

  point_fc <- compute_point_forecasts(x[[dist_col]], point_forecast, response)
  x[names(point_fc)] <- point_fc
  x <- x[unique(c(key, index_var(x), dist_col, names(point_fc), names(x)))]
  build_fable(x, response = response, distribution = dist_col)
}

# Forecasts used to be returned as a fable by the methods for models, which
# is converted to its forecast distribution (or to a list of them for a list
# of models). A list of forecasts is returned as is.
as_forecast_distribution <- function(x) {
  if (is_fable(x)) return(x[[distribution_var(x)]])
  if (is.list(x) && !inherits(x, "distribution")) return(lapply(x, as_forecast_distribution))
  x
}

#' @export
forecast.mdl_df <- mdl_df_method(forecast)

#' @export
forecast.mdl_lst <- function(object, new_data = NULL, key_data, ...){
  # `key_data` is only used by joint forecasting methods for `mdl_lst`
  # subclasses, and so it isn't passed on to each model.
  lapply(
    dispatch_mdl_lst(object, ..., .f = forecast, .new_data = new_data),
    as_forecast_distribution
  )
}

#' @export
forecast.lst_mdl <- deprecate_lst_mdl(forecast.mdl_lst)

# The removed `bias_adjust` argument would otherwise be silently ignored
check_removed_bias_adjust <- function(dots, call = caller_env()) {
  if ("bias_adjust" %in% names(dots)) {
    abort(c(
      "The `bias_adjust` argument of `forecast()` has been removed.",
      i = "Use `point_forecast` to choose the point forecasts, e.g. `point_forecast = list(.median = median)`."
    ), call = call)
  }
}

#' @rdname forecast
#' @export
forecast.mdl_ts <- function(object, new_data = NULL, h = NULL,
                            simulate = FALSE, bootstrap = FALSE, times = 5000,
                            point_forecast = list(), ...){
  check_removed_bias_adjust(list2(...))
  if(!isFALSE(simulate) || !isFALSE(bootstrap)) {
    dots <- list2(...)
    modifier <- deprecated_sim_modifier(
      list(simulate = simulate, bootstrap = bootstrap, times = times, bootstrap_block_size = dots$bootstrap_block_size),
      "forecast", user_env = caller_env()
    )
    object <- modifier(object)
    dots$bootstrap_block_size <- NULL
    return(exec(forecast, object, new_data = new_data, h = h, !!!dots))
  }

  setup <- forecast_mdl_ts_setup(object, new_data, h)
  if(!is.null(setup$empty_fc)) return(setup$empty_fc)
  new_data <- setup$new_data

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

  forecast_mdl_ts_finalise(fc, setup$resp_vars)
}

#' @export
forecast.mdl_ts_sim <- function(object, new_data = NULL, h = NULL,
                                point_forecast = list(), ...){
  check_removed_bias_adjust(list2(...))
  setup <- forecast_mdl_ts_setup(object, new_data, h)
  if(!is.null(setup$empty_fc)) return(setup$empty_fc)
  new_data <- setup$new_data
  resp_vars <- setup$resp_vars

  # The modifier's number of paths is used, rather than the deprecated `times`
  dots <- list2(...)
  dots$times <- NULL
  sim <- exec(generate, object, new_data, times = object%@%"times", !!!dots)
  forecast_from_generate(sim, resp_vars)
}

# The forecast distribution of generate()'s simulated paths, split by date
# into a dist_sample distribution.
forecast_from_generate <- function(sim, resp_vars) {
  fc_idx <- sim[[index_var(sim)]]
  fc <- if (length(resp_vars) > 1) {
    do.call(cbind, sim[resp_vars])
  } else {
    sim[[".sim"]]
  }
  fc <- distributional::dist_sample(vctrs::vec_split(fc, fc_idx)$val)

  forecast_mdl_ts_finalise(fc, resp_vars)
}

# Shared setup for forecast.mdl_ts()/forecast.mdl_ts_sim(): resolves
# new_data/h and the response names.
forecast_mdl_ts_setup <- function(object, new_data, h) {
  if(!is.null(h) && !is.null(new_data)){
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }

  if(is.null(new_data)){
    new_data <- make_future_data(object$data, h)
  }
  check_transformation_data(object, new_data)

  # Reseed lag()'s short term memory and trend()'s origin from this fit.
  restore_model_state(object)

  resp_vars <- vapply(object$response, expr_name, character(1L), USE.NAMES = FALSE)

  # Missing models (see `fill_null_models()`) don't have any data, and give
  # missing forecasts.
  empty_fc <- NULL
  if(NROW(new_data) == 0 || NROW(object$data) == 0){
    empty_fc <- forecast_mdl_ts_finalise(distributional::dist_missing(NROW(new_data)), resp_vars)
  }

  list(new_data = new_data, resp_vars = resp_vars, empty_fc = empty_fc)
}

# The forecast distribution of a model, named by its response(s)
forecast_mdl_ts_finalise <- function(fc, resp_vars) {
  dist_variates(fc) <- resp_vars
  fc
}

compute_point_forecasts <- function(distribution, measures, resp = dist_variates(distribution)){
  map(measures, function(f){
    out <- f(distribution)
    # Empty distributions can give NULL or a vector rather than an empty point
    # forecast with a column for each response (#313)
    if(length(distribution) == 0 && !is.matrix(out) && length(resp) > 1){
      out <- matrix(numeric(), 0, length(resp), dimnames = list(NULL, resp))
    } else if(is.null(out) && length(distribution) == 0){
      out <- numeric()
    }
    out
  })
}

#' @export
forecast.fbl_df <- function(object, ...){
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