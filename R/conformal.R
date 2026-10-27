#' Split conformal prediction intervals
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Produces forecast distributions using split conformal prediction rather
#' than the model's own distributional assumptions. This can give better
#' calibrated intervals when the model's errors are biased or poorly
#' described by its assumed distribution.
#'
#' The forecast distribution is obtained as follows:
#'
#' 1. The model is refit on an expanding window of its history, forecasting
#'    up to `h` steps ahead from each origin to obtain out-of-sample errors.
#' 2. The errors for each forecast horizon are used as its calibration set.
#'    Forecasts for horizons with fewer than `min_calibration` errors can't be
#'    calibrated, and are `NA` (with a warning).
#' 3. The quantiles of the calibration errors are added to the model's point
#'    forecast to give the forecast distribution.
#'
#' @details
#' This differs from textbook split conformal prediction in a few ways:
#'
#' * The calibration errors come from rolling-origin refits of the model
#'   (time series cross-validation) rather than a single split of the data, so
#'   there's no finite-sample coverage guarantee.
#' * The forecast distribution is a [distributional::dist_sample()] of
#'   `n_quantiles` quantiles of the calibration errors (on an evenly spaced
#'   grid of probabilities), added to the model's point forecast. Any interval
#'   can be computed from it with [hilo()], and intervals needn't be
#'   symmetric. The quantiles are the conformal order statistics of the
#'   errors, with those beyond the observed errors truncated to the most
#'   extreme error.
#' * The mean of the forecast distribution (`.mean`) is the model's point
#'   forecast plus the mean of these quantiles, so it is shifted from the
#'   model's own point forecast when its errors are biased.
#'
#' Calibration refits the model for every origin of its history, which can be
#' slow for long series or slow to estimate models. Refits of the first,
#' shortest windows which fail are left out, and warnings from the refits
#' aren't shown. Fitted models only keep their response, so models that use
#' other data (such as exogenous regressors) can't be refit to calibrate them,
#' and aren't supported yet.
#'
#' `conformal_scp()` only changes forecasts. It uses the point forecasts of
#' the model it wraps (without any simulation from [simulate_iid()] or
#' [bootstrap_iid()]), and [generate()] simulates from that model as usual.
#'
#' @param model A fitted model (e.g. `mutate(ets = conformal_scp(ets))`), or a
#' model specification (e.g. `model(ets = conformal_scp(ETS(y)))`).
#' @param n_quantiles The number of quantiles of the calibration errors used to
#' represent each forecast distribution.
#' @param min_calibration The minimum number of out-of-sample errors needed to
#' calibrate a forecast horizon.
#'
#' @seealso [bootstrap_iid()], [simulate_iid()], [forecast()], [refit()]
#'
#' @references
#' Wang, X., & Hyndman, R.J. (2024). \pkg{conformalForecast}: Conformal
#' Prediction for Time Series Forecasting.
#' \url{https://github.com/xqnwang/conformalForecast}
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' library(dplyr)
#' aus_production %>%
#'   slice_tail(n = 44) %>%
#'   model(snaive = SNAIVE(Beer)) %>%
#'   mutate(snaive = conformal_scp(snaive)) %>%
#'   forecast(h = 4)
#'
#' @export
conformal_scp <- function(model, n_quantiles = 5000, min_calibration = 20) {
  check_count(n_quantiles)
  check_count(min_calibration)
  UseMethod("conformal_scp")
}

#' @export
conformal_scp.default <- function(model, n_quantiles = 5000, min_calibration = 20) {
  abort_invalid_modifier(model, "conformal_scp")
}

#' @export
conformal_scp.mdl_ts <- function(model, n_quantiles = 5000, min_calibration = 20) {
  if (!is_null_model(model)) check_conformal_refit(model)
  mdl_wrap(model, "mdl_conformal_scp", n_quantiles = n_quantiles, min_calibration = min_calibration)
}

#' @export
conformal_scp.mdl_lst <- function(model, n_quantiles = 5000, min_calibration = 20) {
  # Each series is calibrated on its own history, not jointly across series.
  map_models(model, conformal_scp, n_quantiles = n_quantiles, min_calibration = min_calibration)
}

#' @export
conformal_scp.mdl_df <- function(model, n_quantiles = 5000, min_calibration = 20) {
  modify_model_group(model, conformal_scp, n_quantiles = n_quantiles, min_calibration = min_calibration)
}

#' @export
conformal_scp.mdl_spec <- function(model, n_quantiles = 5000, min_calibration = 20) {
  mdl_wrap(model, "mdl_spec_conformal_scp", n_quantiles = n_quantiles, min_calibration = min_calibration)
}

# Wrap the mdl_ts produced by estimate() when the spec was itself wrapped
# (model(ets = conformal_scp(ETS(y)))). Dispatches on `model` (see new_model()).
#' @export
new_model.mdl_spec_conformal_scp <- function(fit = NULL, model, data, response,
                                             transformation, recent_data = NULL){
  conformal_scp(NextMethod(), n_quantiles = model %@% "n_quantiles", min_calibration = model %@% "min_calibration")
}

# Models are refit on their own history to calibrate, so the specials must be
# computable from it (this fails for models using regressors).
check_conformal_refit <- function(object, call = caller_env()) {
  dt <- conformal_history(object)
  mdl <- object$model
  restore_model_state(object)
  mdl$stage <- "refit"
  on.exit({
    mdl$remove_data()
    mdl$stage <- NULL
  })
  tryCatch({
    mdl$add_data(dt)
    parse_model_rhs(mdl)
  }, error = function(e) {
    abort(c(
      "`conformal_scp()` can't refit this model to calibrate it.",
      "i" = "Models that use data other than their response (such as exogenous regressors) aren't supported yet."
    ), parent = e, call = call)
  })
  invisible(object)
}

#' @export
forecast.mdl_conformal_scp <- function(object, new_data = NULL, h = NULL,
                                       point_forecast = list(.mean = mean), ...){
  setup <- forecast_mdl_ts_setup(object, new_data, h, point_forecast)
  if(!is.null(setup$empty_fbl)) return(setup$empty_fbl)
  new_data <- setup$new_data
  resp_vars <- setup$resp_vars
  dist_col <- setup$dist_col

  if(length(resp_vars) > 1L){
    abort("conformal_scp() currently only supports models with a single response variable.")
  }

  n_quantiles <- object%@%"n_quantiles"
  min_calibration <- (object%@%"min_calibration") %||% 20

  # Strip our class before using the model to compute the anchor point
  # forecast and calibration residuals, so both use plain analytical
  # forecast()/refit() rather than recursing back into this method (the
  # rolling-origin refits in conformal_scp_calibration() would otherwise
  # try to conformally-calibrate every refit sub-model too).
  base_object <- object
  class(base_object) <- setdiff(class(object), c("mdl_conformal_scp", mdl_sim_classes))

  anchor <- forecast(base_object, new_data = new_data, point_forecast = list(.mean = mean), ...)$.mean

  pools <- conformal_scp_calibration(base_object, NROW(new_data), min_calibration, call = NULL)

  draws <- .mapply(function(a, pool) a + conformal_scp_quantile(pool, n_quantiles),
                   list(anchor, pools), NULL)
  # Horizons that can't be calibrated are NA
  is_na <- vapply(pools, is.null, logical(1L))
  draws[is_na] <- list(NA_real_)
  fc <- distributional::dist_sample(draws)
  fc[is_na] <- NA

  forecast_mdl_ts_assemble(new_data, fc, resp_vars, dist_col, point_forecast)
}

# Rolling-origin (time series cross-validation) nonconformity scores for
# horizons 1:H, as a list with the errors of each horizon. Horizons with fewer
# than `min_calibration` errors can't be calibrated, and are NULL.
conformal_scp_calibration <- function(object, H, min_calibration, call = caller_env()) {
  cv <- conformal_cv_errors(object, H)
  pools <- lapply(seq_len(H), function(h) cv$errors[!is.na(cv$errors[, h]), h])
  n <- lengths(pools)

  if(cv$n_refits == 0L && !is.null(cv$error)) {
    abort("Can't refit the model to calibrate `conformal_scp()`.", parent = cv$error, call = call)
  }
  if(sum(n) < min_calibration){
    abort(c(
      "Not enough held-out data to calibrate `conformal_scp()`.",
      "i" = sprintf(
        "At least %d out-of-sample errors are required for calibration (only %d were available after rolling-origin refitting).",
        min_calibration, sum(n)
      )
    ), call = call)
  }
  if(cv$n_failed > 0L) {
    warn(c(
      sprintf("%d of the rolling-origin refits for calibrating `conformal_scp()` failed, and were left out.", cv$n_failed),
      "x" = conditionMessage(cv$failure)
    ))
  }

  short <- which(n < min_calibration)
  if(length(short) > 0L) {
    warn(c(
      sprintf(
        "Forecasts for %s are `NA`, as there are fewer than %d out-of-sample errors to calibrate them with `conformal_scp()`.",
        format_horizons(short), min_calibration
      ),
      "i" = "Use a shorter forecast horizon, or a smaller `min_calibration`."
    ))
  }
  pools[short] <- list(NULL)
  pools
}

format_horizons <- function(h) {
  if(length(h) == 1L) return(paste("horizon", h))
  if(all(diff(h) == 1L)) return(sprintf("horizons %d to %d", h[1], h[length(h)]))
  paste("horizons", paste(h, collapse = ", "))
}

# Evaluate the split conformal quantile function of the nonconformity scores
# `errors` on an evenly spaced grid of `n_quantiles` probabilities. With n scores,
# the finite-sample valid conformal bounds for a central interval at level
# 1 - alpha are the floor((n+1)*alpha/2)-th and ceiling((n+1)*(1-alpha/2))-th
# order statistics (i.e. the empirical quantiles of the scores augmented with
# -Inf/+Inf), so the lower tail uses floor() and the upper tail ceiling().
# Order statistics beyond the observed scores (which conformal prediction
# would make infinite) are truncated to the most extreme observed score.
conformal_scp_quantile <- function(errors, n_quantiles) {
  errors <- sort(errors)
  n <- length(errors)
  p <- (seq_len(n_quantiles) - 1) / max(n_quantiles - 1, 1)
  k <- ifelse(p < 0.5, floor((n + 1) * p), ceiling((n + 1) * p))
  errors[pmin(pmax(k, 1), n)]
}

# Out-of-sample 1:H-step-ahead forecast errors from refit()ting the model on
# an expanding window of its own history. This doesn't use hfitted(), as model
# specific hfitted() methods (such as for ETS) keep the parameters estimated
# from the whole series. Each origin is refit once and forecast up to H steps
# ahead. The errors are returned as an n x H matrix, where row t, column h is
# the error of the h-step forecast of time t (NA where no such forecast
# exists), with the number of successful and failed refits, the first error
# and the last failure after a successful refit.
conformal_cv_errors <- function(object, H) {
  dt <- conformal_history(object)
  resp <- response_vars(object)
  n <- nrow(dt)
  errors <- matrix(NA_real_, nrow = n, ncol = H)

  actual <- dt[[resp]]
  # Errors are only computed for time points within the history, so each
  # origin's future data is a slice of the history's index (cheaper than
  # constructing it with make_future_data() for every origin).
  future <- dt[c(key_vars(dt), index_var(dt), model_transformation_params(object))]
  n_refits <- n_failed <- 0L
  error <- failure <- NULL
  for (i in seq_len(n - 1L)) {
    h <- seq_len(min(H, n - i))
    # Slicing a single row loses the tsibble's interval, which is required
    # to compute some specials (e.g. seasonal periods).
    new_data <- vec_slice(future, i + h)
    attr(new_data, "interval") <- interval(future)
    # Warnings from refitting short windows of the history aren't shown
    fc <- tryCatch(withCallingHandlers({
      mdl <- refit(object, vec_slice(dt, seq_len(i)), reestimate = TRUE)
      forecast(mdl, new_data = new_data, point_forecast = list(.mean = mean))
    }, warning = function(w) invokeRestart("muffleWarning")), error = function(e) e)
    if (inherits(fc, "error")) {
      # Refits of the first (shortest) windows are expected to fail for some
      # models, but failures after a successful refit are reported.
      if (n_refits > 0L) {
        n_failed <- n_failed + 1L
        failure <- fc
      }
      error <- error %||% fc
      next
    }
    n_refits <- n_refits + 1L
    errors[cbind(i + h, h)] <- actual[i + h] - fc$.mean
  }
  list(errors = errors, n_refits = n_refits, n_failed = n_failed, error = error, failure = failure)
}

# The model's history on the response scale, as refit() expects
conformal_history <- function(object) {
  dt <- object$data
  bt <- lapply(object$transformation, function(x) {
    invert_transformation(bind_transformation_data(x, dt))
  })
  mv <- match(model_response_cols(object), names(dt))
  dt[mv] <- mapply(calc, bt, dt[mv], SIMPLIFY = FALSE)
  names(dt)[mv] <- response_vars(object)
  dt
}
