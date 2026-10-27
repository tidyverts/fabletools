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
#' Applied to a model group (see [model_groups]), sampling is joint across
#' every model of the group, for all of its series. The innovations of each
#' model are then its own residuals at the same historical time points, which
#' keeps the relationship between the models' (and series') errors.
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
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
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
  check_count(times)
  UseMethod("bootstrap_iid")
}

#' @export
bootstrap_iid.default <- function(model, times = 5000) {
  abort_invalid_modifier(model, "bootstrap_iid")
}

#' @export
bootstrap_iid.mdl_ts <- function(model, times = 5000) {
  mdl_wrap(model, c("mdl_bootstrap_iid", "mdl_ts_sim"), times = times)
}

#' @export
bootstrap_iid.mdl_lst <- function(model, times = 5000) {
  mdl_wrap_col(map_models(model, bootstrap_iid, times = times), "mdl_lst_bootstrap_iid")
}

#' @export
bootstrap_iid.mdl_df <- function(model, times = 5000) {
  check_model_group(model, "bootstrap_iid")
  mdl_wrap_col(map_models(model, bootstrap_iid, times = times), "mdl_df_bootstrap_iid")
}

#' @export
bootstrap_iid.mdl_spec <- function(model, times = 5000) {
  mdl_wrap(model, "mdl_spec_bootstrap_iid", times = times)
}

# Wrap the mdl_ts produced by estimate() when the spec was itself wrapped
# (model(ets = bootstrap_iid(ETS(y)))). Dispatches on `model` (see new_model()).
#' @export
new_model.mdl_spec_bootstrap_iid <- function(fit = NULL, model, data, response,
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
  check_count(times)
  check_count(block_size, allow_null = TRUE)
  UseMethod("bootstrap_block")
}

#' @export
bootstrap_block.default <- function(model, times = 5000, block_size = NULL) {
  abort_invalid_modifier(model, "bootstrap_block")
}

#' @export
bootstrap_block.mdl_ts <- function(model, times = 5000, block_size = NULL) {
  block_size <- block_size %||% default_block_size(list(model))
  mdl_wrap(
    model, c("mdl_bootstrap_block", "mdl_ts_sim"),
    times = times, block_size = block_size
  )
}

#' @export
bootstrap_block.mdl_df <- function(model, times = 5000, block_size = NULL) {
  check_model_group(model, "bootstrap_block")
  block_size <- block_size %||% default_block_size(model)
  model <- map_models(model, bootstrap_block, times = times, block_size = block_size)
  mdl_wrap_col(model, "mdl_df_bootstrap_block", block_size = block_size)
}

# The largest seasonal period of the models' data
default_block_size <- function(x) {
  freq <- vapply(model_list(x), function(m) stats::frequency(m$data), double(1L))
  max(1L, as.integer(round(max(freq, 1))))
}

#' @export
bootstrap_block.mdl_spec <- function(model, times = 5000, block_size = NULL) {
  mdl_wrap(model, "mdl_spec_bootstrap_block", times = times, block_size = block_size)
}

#' @export
new_model.mdl_spec_bootstrap_block <- function(fit = NULL, model, data, response,
                                               transformation, recent_data = NULL){
  bootstrap_block(NextMethod(), times = model %@% "times", block_size = model %@% "block_size")
}

#' @export
bootstrap_block.mdl_lst <- function(model, times = 5000, block_size = NULL) {
  block_size <- block_size %||% default_block_size(model)
  model <- map_models(model, bootstrap_block, times = times, block_size = block_size)
  mdl_wrap_col(model, "mdl_lst_bootstrap_block", block_size = block_size)
}

# --- Joint bootstrap sampling ----------------------------------------------
# Draws the same historical time point for every model of a column (across
# series) or group (across series and models) at a given position, preserving
# their correlation. The shared draws are attached to each model, which then
# dispatches as usual (through any reconciliation).

#' @export
forecast.mdl_lst_bootstrap_iid <- function(object, new_data = NULL, h = NULL, ...) {
  object <- bootstrap_joint(object, new_data, h)
  NextMethod()
}

#' @export
forecast.mdl_lst_bootstrap_block <- function(object, new_data = NULL, h = NULL, ...) {
  object <- bootstrap_joint(object, new_data, h, block_size = object%@%"block_size")
  NextMethod()
}

#' @export
forecast.mdl_df_bootstrap_iid <- forecast.mdl_lst_bootstrap_iid

#' @export
forecast.mdl_df_bootstrap_block <- forecast.mdl_lst_bootstrap_block

#' @export
generate.mdl_lst_bootstrap_iid <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...) {
  if (!is.null(seed)) {
    RNGstate <- use_seed(seed)
    on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
  }
  x <- bootstrap_joint(x, new_data, h, times = times)
  NextMethod()
}

#' @export
generate.mdl_lst_bootstrap_block <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...) {
  if (!is.null(seed)) {
    RNGstate <- use_seed(seed)
    on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
  }
  x <- bootstrap_joint(x, new_data, h, times = times, block_size = x%@%"block_size")
  NextMethod()
}

#' @export
generate.mdl_df_bootstrap_iid <- generate.mdl_lst_bootstrap_iid

#' @export
generate.mdl_df_bootstrap_block <- generate.mdl_lst_bootstrap_block

# Draw the time points to bootstrap from jointly for the models of a column or
# group, and attach them to each model. `new_data` has the data for each row.
# NULL models are left out (and give NA), and models already given joint draws
# (by an outer modifier) are kept as is. `times` is the number of paths,
# defaulting to each model's own.
bootstrap_joint <- function(x, new_data, h, times = NULL, block_size = NULL, call = caller_env()) {
  models <- model_list(x)
  # The models of a group are listed by column, each with a model for every row
  new_data <- rep_len(new_data %||% list(NULL), length(models))
  use <- !vapply(models, is_null_model, logical(1L))
  if (!any(use) || all(vapply(models[use], has_bootstrap_draws, logical(1L)))) {
    return(x)
  }
  new_data <- .mapply(function(m, nd) {
    replicate_new_data(nd %||% make_future_data(m$data, h), times %||% (m%@%"times") %||% 1)
  }, list(models[use], new_data[use]), NULL)
  shape <- unique(lapply(new_data, function(nd) lengths(tsibble::key_rows(nd))))
  if (length(shape) > 1L) {
    abort("Joint bootstrap sampling requires all models to share the same forecast horizon and number of paths.", call = call)
  }
  draws <- bootstrap_draw_times(models[use], new_data[[1]], block_size, call = call)
  map_models(x, function(m) if (is_null_model(m)) m else structure(m, bootstrap_draws = draws))
}

has_bootstrap_draws <- function(x) !is.null(attr(x, "bootstrap_draws"))
