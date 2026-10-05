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
#' 2. The errors for each forecast horizon are used as calibration sets.
#'    Horizons with fewer than `min_calibration` errors instead use the errors
#'    from all horizons combined.
#' 3. The quantiles of the calibration errors are added to the model's point
#'    forecast to give the forecast distribution.
#'
#' @param model A fitted model (e.g. `mutate(ets = conformal_scp(ets))`), or a
#' model specification (e.g. `model(ets = conformal_scp(ETS(y)))`).
#' @param times The number of quantiles used to represent each forecast
#' distribution.
#' @param min_calibration The minimum number of errors needed to calibrate a
#' forecast horizon.
#'
#' @seealso [bootstrap_iid()], [simulate_iid()], [forecast()], [refit()]
#'
#' @references
#' Wang, X., & Hyndman, R.J. (2024). \pkg{conformalForecast}: Conformal
#' Prediction for Time Series Forecasting.
#' \url{https://github.com/xqnwang/conformalForecast}
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(dplyr)
#' aus_production %>%
#'   model(ets = ETS(Beer)) %>%
#'   mutate(ets = conformal_scp(ets)) %>%
#'   forecast()
#'
#' @export
conformal_scp <- function(model, times = 5000, min_calibration = 20) {
  UseMethod("conformal_scp")
}

#' @export
conformal_scp.mdl_ts <- function(model, times = 5000, min_calibration = 20) {
  mdl_wrap(model, "mdl_conformal_scp", times = times, min_calibration = min_calibration)
}

#' @export
conformal_scp.mdl_lst <- function(model, times = 5000, min_calibration = 20) {
  # Independent (mdl_ts) dispatch level only - each series in the column is
  # calibrated on its own history, with no joint mdl_lst-level calibration
  # across series (unlike bootstrap_iid()'s joint sampling). See design notes
  # in _dev/model-modifiers.md.
  new_mdl_lst(lapply(
    vctrs::vec_data(model), conformal_scp, times = times, min_calibration = min_calibration
  ))
}

#' @export
conformal_scp.mdl_defn <- function(model, times = 5000, min_calibration = 20) {
  mdl_wrap(model, "mdl_defn_conformal_scp", times = times, min_calibration = min_calibration)
}

# Wrap the mdl_ts produced by estimate() when the spec was itself wrapped
# (model(ets = conformal_scp(ETS(y)))). Dispatches on `model` (see new_model()).
new_model.mdl_defn_conformal_scp <- function(fit = NULL, model, data, response,
                                             transformation, recent_data = NULL){
  conformal_scp(NextMethod(), times = model %@% "times", min_calibration = model %@% "min_calibration")
}

#' @export
forecast.mdl_conformal_scp <- function(object, new_data = NULL, h = NULL, times = NULL,
                                       point_forecast = list(.mean = mean), ...){
  setup <- forecast_mdl_ts_setup(object, new_data, h)
  if(!is.null(setup$empty_fbl)) return(setup$empty_fbl)
  new_data <- setup$new_data
  resp_vars <- setup$resp_vars
  dist_col <- setup$dist_col

  if(length(resp_vars) > 1L){
    abort("conformal_scp() currently only supports models with a single response variable.")
  }

  times <- times %||% (object%@%"times") %||% 5000
  min_calibration <- (object%@%"min_calibration") %||% 20

  # Strip our class before using the model to compute the anchor point
  # forecast and calibration residuals, so both use plain analytical
  # forecast()/refit() rather than recursing back into this method (the
  # rolling-origin refits in conformal_scp_calibration() would otherwise
  # try to conformally-calibrate every refit sub-model too).
  base_object <- object
  class(base_object) <- setdiff(class(object), "mdl_conformal_scp")

  anchor <- forecast(base_object, new_data = new_data, point_forecast = list(.mean = mean), ...)$.mean

  pools <- conformal_scp_calibration(base_object, NROW(new_data), min_calibration)

  draws <- .mapply(function(a, pool) a + conformal_scp_quantile(pool, times),
                   list(anchor, pools), NULL)
  fc <- distributional::dist_sample(draws)

  forecast_mdl_ts_assemble(new_data, fc, resp_vars, dist_col, point_forecast)
}

# Rolling-origin (time series cross-validation) nonconformity scores for
# horizons 1:H. Returns a length-H list of numeric residual pools (one per
# horizon), falling back to a pooled set (across all horizons) for any
# horizon with fewer than `min_calibration` out-of-sample errors.
conformal_scp_calibration <- function(object, H, min_calibration) {
  errors <- conformal_cv_errors(object, H)
  pools <- lapply(seq_len(H), function(h) errors[!is.na(errors[, h]), h])

  if(max(lengths(pools), 0L) < min_calibration){
    abort(c(
      "Not enough held-out data to calibrate `conformal_scp()`.",
      "i" = sprintf(
        "At least %d out-of-sample errors are required for calibration (only %d were available after rolling-origin refitting).",
        min_calibration, max(lengths(pools), 0L)
      )
    ))
  }

  pooled <- unlist(pools, use.names = FALSE)
  lapply(pools, function(p) if(length(p) >= min_calibration) p else pooled)
}

# Evaluate the split conformal quantile function of the nonconformity scores
# `errors` on an evenly spaced grid of `times` probabilities. With n scores,
# the finite-sample valid conformal bounds for a central interval at level
# 1 - alpha are the floor((n+1)*alpha/2)-th and ceiling((n+1)*(1-alpha/2))-th
# order statistics (i.e. the empirical quantiles of the scores augmented with
# -Inf/+Inf), so the lower tail uses floor() and the upper tail ceiling().
# Order statistics beyond the observed scores (which conformal prediction
# would make infinite) are truncated to the most extreme observed score.
conformal_scp_quantile <- function(errors, times) {
  errors <- sort(errors)
  n <- length(errors)
  p <- (seq_len(times) - 1) / max(times - 1, 1)
  k <- ifelse(p < 0.5, floor((n + 1) * p), ceiling((n + 1) * p))
  errors[pmin(pmax(k, 1), n)]
}

# Genuinely out-of-sample 1:H-step-ahead forecast errors via repeated refit()
# on an expanding window of `object`'s own history (adapted from the manual
# refit loop in hfitted.mdl_ts(), but used unconditionally - see Description
# above for why conformal_scp() can't use hfitted() itself for this). Each
# origin is refit once and forecast up to H steps ahead, filling every
# horizon's errors from that single refit. Returns an n x H matrix of errors,
# where row t, column h is the error of the h-step forecast of time t (made
# at origin t - h), and NA where no such forecast exists.
conformal_cv_errors <- function(object, H) {
  dt <- object$data
  resp <- response_vars(object)
  n <- nrow(dt)
  errors <- matrix(NA_real_, nrow = n, ncol = H)

  # Undo transformations, so refit() (which expects response-scale data) can
  # be given a plain prefix of the original history.
  bt <- lapply(object$transformation, function(x) {
    invert_transformation(bind_transformation_data(x, dt))
  })
  mv <- match(model_response_cols(object), names(dt))
  dt[mv] <- mapply(calc, bt, dt[mv], SIMPLIFY = FALSE)
  names(dt)[mv] <- resp

  actual <- dt[[resp]]
  # Errors are only computed for time points within the history, so each
  # origin's future data is a slice of the history's index (cheaper than
  # constructing it with make_future_data() for every origin).
  future <- dt[c(key_vars(dt), index_var(dt), model_transformation_params(object))]
  for (i in seq_len(n - 1L)) {
    h <- seq_len(min(H, n - i))
    mdl <- tryCatch(
      refit(object, vec_slice(dt, seq_len(i)), reestimate = TRUE),
      error = function(e) NULL
    )
    if (is.null(mdl)) next
    # Slicing a single row loses the tsibble's interval, which is required
    # to compute some specials (e.g. seasonal periods).
    new_data <- vec_slice(future, i + h)
    attr(new_data, "interval") <- interval(future)
    fc <- tryCatch(
      forecast(mdl, new_data = new_data, point_forecast = list(.mean = mean)),
      error = function(e) NULL
    )
    if (is.null(fc)) next
    errors[cbind(i + h, h)] <- actual[i + h] - fc$.mean
  }
  errors
}
