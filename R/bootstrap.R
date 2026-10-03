#' Bootstrap forecast sample paths
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Wraps a fitted model (or a model column from a mable) so that [forecast()]
#' produces its distribution from bootstrapped sample paths instead of
#' closed-form/analytical results, and so that [generate()] draws its
#' innovations from the model's own fitted residuals instead of the model's
#' assumed error distribution. This allows the forecast distribution to take
#' a different shape to the model's theoretical one, which can better
#' reflect asymmetry or heavy tails in the data.
#'
#' `bootstrap_iid()` resamples residuals independently (with replacement) for
#' each simulated time point. `bootstrap_block()` instead resamples
#' contiguous blocks of residuals, preserving any residual autocorrelation
#' the model hasn't captured.
#'
#' Applied to a `mdl_lst` (a model column spanning multiple series/keys),
#' `bootstrap_iid()`/`bootstrap_block()` sample **jointly** across the series:
#' the same historical time point is drawn for every series at a given
#' (replicate, forecast-step) position, so the historical cross-sectional
#' relationship between series' residuals carries over into the simulated
#' innovations, rather than resampling each series as if it were independent
#' of the others. This matters most for hierarchies (see [aggregate_key()]/
#' [reconcile_mint()]): jointly bootstrapping respects the observed correlation
#' between a hierarchy's nodes, instead of treating every node as unrelated.
#' If the series don't all share the same historical time domain, sampling is
#' restricted to their overlapping period and a warning is raised.
#'
#' @param model A fitted model, typically a model column from a mable
#' modified with [dplyr::mutate()] (e.g. `mutate(ets = bootstrap_iid(ets))`).
#' Alternatively, an unfitted model specification can be wrapped directly
#' inside [model()] (e.g. `model(ets = bootstrap_iid(ETS(Trips)))`), in which
#' case each series is fitted and bootstrapped independently - unlike wrapping
#' an already-fitted `mdl_lst` column (which samples **jointly** across
#' series, see Description).
#' @param times The number of bootstrapped sample paths to simulate.
#'
#' @seealso [simulate_iid()], [forecast()], [generate()]
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(dplyr)
#' aus_production %>%
#'   model(ets = ETS(Beer)) %>%
#'   mutate(ets = bootstrap_iid(ets, times = 1000)) %>%
#'   forecast()
#'
#' # Applied to the model specification before fitting, each series is
#' # bootstrapped independently (no joint cross-sectional sampling):
#' aus_production %>%
#'   model(ets = bootstrap_iid(ETS(Beer), times = 1000)) %>%
#'   forecast()
#'
#' @export
bootstrap_iid <- function(model, times = 5000) {
  UseMethod("bootstrap_iid")
}

#' @export
bootstrap_iid.mdl_ts <- function(model, times = 5000) {
  mdl_wrap(model, c("mdl_bootstrap_iid", "mdl_ts_sim"), times = times)
}

#' @export
bootstrap_iid.mdl_lst <- function(model, times = 5000) {
  wrapped <- new_mdl_lst(lapply(vctrs::vec_data(model), bootstrap_iid, times = times))
  structure(wrapped, class = c("mdl_lst_bootstrap_iid", class(wrapped)), times = times)
}

#' @export
bootstrap_iid.mdl_defn <- function(model, times = 5000) {
  mdl_wrap(model, "mdl_defn_bootstrap_iid", times = times)
}

# Wrap the mdl_ts produced by estimate() when the spec was itself wrapped
# (model(ets = bootstrap_iid(ETS(y)))). Dispatches on `model` (see new_model()).
new_model.mdl_defn_bootstrap_iid <- function(fit = NULL, model, data, response,
                                             transformation, recent_data = NULL){
  bootstrap_iid(NextMethod(), times = model %@% "times")
}

#' @param block_size The number of contiguous residuals resampled together in
#' each bootstrap draw. Defaults to the data's seasonal period (via
#' `stats::frequency()`), which preserves within-season residual
#' autocorrelation; use `block_size = 1` for the same behaviour as
#' [bootstrap_iid()].
#' @rdname bootstrap_iid
#' @export
bootstrap_block <- function(model, times = 5000, block_size = NULL) {
  UseMethod("bootstrap_block")
}

#' @export
bootstrap_block.mdl_ts <- function(model, times = 5000, block_size = NULL) {
  block_size <- block_size %||% max(1L, as.integer(round(stats::frequency(model$data))))
  mdl_wrap(
    model, c("mdl_bootstrap_block", "mdl_ts_sim"),
    times = times, block_size = block_size
  )
}

#' @export
bootstrap_block.mdl_defn <- function(model, times = 5000, block_size = NULL) {
  mdl_wrap(model, "mdl_defn_bootstrap_block", times = times, block_size = block_size)
}

new_model.mdl_defn_bootstrap_block <- function(fit = NULL, model, data, response,
                                               transformation, recent_data = NULL){
  bootstrap_block(NextMethod(), times = model %@% "times", block_size = model %@% "block_size")
}

#' @export
bootstrap_block.mdl_lst <- function(model, times = 5000, block_size = NULL) {
  elements <- vctrs::vec_data(model)
  block_size <- block_size %||%
    max(1L, as.integer(round(max(vapply(elements, function(m) stats::frequency(m$data), double(1L))))))
  wrapped <- new_mdl_lst(lapply(elements, bootstrap_block, times = times, block_size = block_size))
  structure(wrapped, class = c("mdl_lst_bootstrap_block", class(wrapped)), times = times, block_size = block_size)
}

# --- Joint (mdl_lst-level) bootstrap sampling -------------------------------
# Draws the same historical time point for every series at a given position,
# preserving cross-sectional correlation.

#' @export
forecast.mdl_lst_bootstrap_iid <- function(object, new_data = NULL, key_data, h = NULL,
                                           times = NULL, point_forecast = list(.mean = mean), ...) {
  times <- times %||% (object%@%"times") %||% 5000
  elements <- vctrs::vec_data(object)
  new_data <- resolve_new_data_list(elements, new_data, h)
  sims <- generate(object, new_data = new_data, times = times, ...)
  forecast_from_joint_generate(elements, new_data, sims, point_forecast)
}

#' @export
forecast.mdl_lst_bootstrap_block <- function(object, new_data = NULL, key_data, h = NULL,
                                             times = NULL, point_forecast = list(.mean = mean), ...) {
  times <- times %||% (object%@%"times") %||% 5000
  elements <- vctrs::vec_data(object)
  new_data <- resolve_new_data_list(elements, new_data, h)
  sims <- generate(object, new_data = new_data, times = times, ...)
  forecast_from_joint_generate(elements, new_data, sims, point_forecast)
}

# Resolve a new_data list per element (filling NULLs via
# make_future_data()), without replicating into times copies.
resolve_new_data_list <- function(elements, new_data, h) {
  new_data <- new_data %||% rep(list(NULL), length.out = length(elements))
  .mapply(function(el, nd) nd %||% make_future_data(el$data, h), list(elements, new_data), NULL)
}

# Turn each element's generate() output into a forecast fable via
# forecast_from_generate().
forecast_from_joint_generate <- function(elements, new_data, sims, point_forecast) {
  Map(function(el, nd, sim) {
    resp_vars <- vapply(el$response, expr_name, character(1L), USE.NAMES = FALSE)
    dist_col <- if (length(resp_vars) > 1) ".distribution" else resp_vars
    forecast_from_generate(nd, sim, resp_vars, dist_col, point_forecast)
  }, elements, new_data, sims)
}

#' @export
generate.mdl_lst_bootstrap_iid <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...) {
  elements <- vctrs::vec_data(x)
  new_data <- bootstrap_joint_new_data(elements, new_data, h, times)
  map2(elements, new_data, generate, times = times, seed = seed, ...)
}

#' @export
generate.mdl_lst_bootstrap_block <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...) {
  elements <- vctrs::vec_data(x)
  new_data <- bootstrap_joint_new_data(elements, new_data, h, times, block_size = x%@%"block_size")
  map2(elements, new_data, generate, times = times, seed = seed, ...)
}

# Align models' residuals onto their overlapping time domain (warning if
# they differ), so index i means the same calendar time for all.
bootstrap_joint_residuals <- function(models) {
  res <- lapply(models, bootstrap_residual_index)
  overlap <- sort(Reduce(intersect, lapply(res, `[[`, "index")))
  full_match <- all(vapply(res, function(r) identical(sort(r$index), overlap), logical(1L)))
  if (!full_match) {
    warn(c(
      "Series being bootstrapped jointly have different residual time domains.",
      "i" = "Joint bootstrap sampling has been restricted to their overlapping period."
    ))
  }
  lapply(res, function(r) {
    pos <- vctrs::vec_match(overlap, r$index)
    if (is.matrix(r$resid)) r$resid[pos, , drop = FALSE] else r$resid[pos]
  })
}

# Build each element's replicated new_data with a shared .innov draw so
# innovations preserve cross-series correlation.
bootstrap_joint_new_data <- function(elements, new_data, h, times, block_size = NULL) {
  new_data_list <- new_data %||% rep(list(NULL), length.out = length(elements))
  new_data_list <- .mapply(function(el, nd) {
    replicate_new_data(nd %||% make_future_data(el$data, h), times)
  }, list(elements, new_data_list), NULL)

  n <- vapply(new_data_list, nrow, integer(1L))
  if (length(unique(n)) > 1L) {
    abort("Joint bootstrap sampling requires all series to share the same forecast horizon and number of paths.")
  }

  res_pool <- bootstrap_joint_residuals(elements)
  n_overlap <- NROW(res_pool[[1]])
  draws <- if (is.null(block_size)) {
    sample.int(n_overlap, n[[1]], replace = TRUE)
  } else {
    if (any(vapply(elements, function(x) any(has_gaps(x$data)$.gaps), logical(1L)))) {
      abort("Residuals must be regularly spaced without gaps to use a block bootstrap method.")
    }
    kr <- tsibble::key_rows(new_data_list[[1]])
    vec_c(!!!lapply(lengths(kr), function(sz) block_bootstrap(n_overlap, block_size, size = sz)))
  }

  Map(function(nd, res) {
    nd$.innov <- if (is.matrix(res)) res[draws, , drop = FALSE] else res[draws]
    nd
  }, new_data_list, res_pool)
}
