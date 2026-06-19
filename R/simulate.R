#' Simulate forecast sample paths
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Wraps a fitted model (or a model column from a mable) so that [forecast()]
#' produces its distribution from simulated sample paths (via [generate()])
#' instead of closed-form/analytical results. Innovations are drawn
#' independently at each time point, parametrically from the model's assumed
#' error distribution, identically to calling `generate()` on the model
#' directly. This is primarily useful for models (or transformations) whose
#' forecast distribution has no closed form, and for producing forecasts
#' that are directly comparable to [bootstrap_iid()]/[bootstrap_block()]
#' (which only differ in how innovations are drawn).
#'
#' @param model A fitted model, typically a model column from a mable
#' modified with [dplyr::mutate()] (e.g. `mutate(ets = simulate_iid(ets))`).
#' Alternatively, an unfitted model specification can be wrapped directly
#' inside [model()] (e.g. `model(ets = simulate_iid(ETS(Trips)))`), in which
#' case each series is fitted and simulated independently, identically to
#' wrapping each series' `mdl_ts` individually.
#' @param times The number of simulated sample paths to generate.
#'
#' @seealso [bootstrap_iid()], [bootstrap_block()], [forecast()], [generate()]
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(dplyr)
#' aus_production %>%
#'   model(ets = ETS(Beer)) %>%
#'   mutate(ets = simulate_iid(ets, times = 1000)) %>%
#'   forecast()
#'
#' # Equivalently, applied to the model specification before fitting:
#' aus_production %>%
#'   model(ets = simulate_iid(ETS(Beer), times = 1000)) %>%
#'   forecast()
#'
#' @export
simulate_iid <- function(model, times = 5000) {
  UseMethod("simulate_iid")
}

#' @export
simulate_iid.mdl_ts <- function(model, times = 5000) {
  mdl_wrap(model, "mdl_ts_sim", times = times)
}

#' @export
simulate_iid.mdl_lst <- function(model, times = 5000) {
  new_mdl_lst(lapply(vctrs::vec_data(model), simulate_iid, times = times))
}

#' @export
simulate_iid.mdl_defn <- function(model, times = 5000) {
  mdl_wrap(model, "mdl_defn_sim", times = times)
}

new_model.mdl_defn_sim <- function(fit = NULL, model, data, response,
                                   transformation, recent_data = NULL){
  simulate_iid(NextMethod(), times = model %@% "times")
}
