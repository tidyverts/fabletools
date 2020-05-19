#' Augment a mable
#' 
#' Uses a fitted model to augment the response variable with fitted values and
#' residuals.
#' 
#' @param x A mable.
#' @param ... Arguments for model methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' # Forecasting with an ETS(M,Ad,A) model to Australian beer production
#' aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   augment(type = "response")
#' }
#' 
#' @rdname augment
#' @export
augment.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(mable_vars(x)))
  kv <- key_vars(x)
  x <- transmute(as_tibble(x), !!!syms(kv), !!sym(".model"),
                 aug = map(!!sym(".fit"), augment, ...))
  unnest_tsbl(x, "aug", parent_key = kv)
}

#' @rdname augment
#' @export
augment.mdl_ts <- function(x, ...){
  tryCatch(augment(x[["fit"]], ...),
           error = function(e){
             idx <- index_var(x$data)
             resp <- x$response
             if(length(resp) > 1){
               response(x) %>% 
                 gather(".response", "value", !!!resp, factor_key = TRUE) %>% 
                 left_join(
                   gather(fitted(x, ...), ".response", ".fitted",
                          !!!resp, factor_key = TRUE),
                   by = c(".response", idx)
                 ) %>% 
                 left_join(
                   gather(residuals(x, ...), ".response", ".resid",
                          !!!resp, factor_key = TRUE),
                   by = c(".response", idx)
                 )
             } else {
               mutate(
                 set_names(response(x), c(idx, as_string(resp[[1]]))),
                 .fitted = fitted(x, ...)[[".fitted"]],
                 .resid = residuals(x, ...)[[".resid"]]
               )
             }
             
           })
}

#' Glance a mable
#' 
#' Uses the models within a mable to produce a one row summary of their fits.
#' This typically contains information about the residual variance,
#' information criterion, and other relevant summary statistics. Each model 
#' will be represented with a row of output.
#' 
#' @param x A mable.
#' @param ... Arguments for model methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' olympic_running %>%
#'   model(lm = TSLM(log(Time) ~ trend())) %>% 
#'   glance()
#' }
#' @rdname glance
#' @export
glance.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(mable_vars(x)))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), glanced = map(!!sym(".fit"), glance))
  unnest_tbl(x, "glanced")
}

#' @rdname glance
#' @export
glance.mdl_ts <- function(x, ...){
  glance(x$fit, ...)
}

#' Extract model coefficients from a mable
#' 
#' This function will obtain the coefficients (and associated statistics) for
#' each model in the mable.
#' 
#' @param x,object A mable.
#' @param ... Arguments for model methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' olympic_running %>%
#'   model(lm = TSLM(log(Time) ~ trend())) %>% 
#'   tidy()
#' }
#' 
#' @rdname tidy
#' @export
tidy.mdl_df <- function(x, ...){
  x <- gather(x, ".model", ".fit", !!!syms(mable_vars(x)))
  keys <- key(x)
  x <- transmute(as_tibble(x),
                 !!!keys, !!sym(".model"), tidied = map(!!sym(".fit"), tidy))
  unnest_tbl(x, "tidied")
}

#' @rdname tidy
#' @export
coef.mdl_df <- function(object, ...){
  tidy(object, ...)
}

#' @rdname tidy
#' @export
tidy.mdl_ts <- function(x, ...){
  tidy(x$fit, ...)
}

#' @rdname tidy
#' @export
coef.mdl_ts <- function(object, ...){
  tidy(object, ...)
}
