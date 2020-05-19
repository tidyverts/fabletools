#' Extract components from a fitted model
#' 
#' Allows you to extract elements of interest from the model which can be
#' useful in understanding how they contribute towards the overall fitted values.
#' 
#' A dable will be returned, which will allow you to easily plot the components
#' and see the way in which components are combined to give forecasts.
#' 
#' @param object A mable.
#' @param ... Other arguments passed to methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' # Forecasting with an ETS(M,Ad,A) model to Australian beer production
#' aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   components() %>% 
#'   autoplot()
#' }
#'
#' @rdname components
#' @export
components.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!syms(mable_vars(object)))
  kv <- key_vars(object)
  object <- transmute(as_tibble(object),
                      !!!syms(kv), !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  attrs <- combine_dcmp_attr(object[["cmp"]])
  object <- unnest_tsbl(object, "cmp", parent_key = kv)
  as_dable(object, method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

#' @rdname components
#' @export
components.mdl_ts <- function(object, ...){
  components(object$fit, ...)
}