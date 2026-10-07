#' Extract components from a fitted model
#' 
#' Allows you to extract elements of interest from the model which can be
#' useful in understanding how they contribute towards the overall fitted values.
#' 
#' A dable will be returned, which will allow you to easily plot the components
#' and see the way in which components are combined to give forecasts.
#' 
#' The components can also be visualised using the [`autoplot()`] method provided
#' by the ggtime package.
#' 
#' @param object A mable.
#' @param ... Other arguments passed to methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' 
#' # Forecasting with an ETS(M,Ad,A) model to Australian beer production
#' aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   components()
#' 
#' @rdname components
#' @export
components.mbl_df <- function(object, ...){
  if(NROW(object) == 0) {
    abort("Can't compute components for a mable without any models, as the structure of the decomposition depends on the models.")
  }
  dispatch_mbl_df(object, ..., .f = components, .values_to = ".cmp",
                  .unnest = unnest_dable)
}

#' @export
components.mdl_df <- mdl_df_method(components)

#' @export
components.mdl_lst <- mdl_lst_method(components)

# Combine the nested decompositions of each model into a dable
unnest_dable <- function(x, col, key) {
  attrs <- combine_dcmp_attr(x[[col]])
  x <- unnest_tsbl(x, col, parent_key = key)
  as_dable(x, method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

#' @rdname components
#' @export
components.mdl_ts <- function(object, ...){
  components(object$fit, ...)
}