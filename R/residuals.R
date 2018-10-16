#' @importFrom stats residuals
#' @export
residuals.mable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              residuals = map(!!sym("model"), residuals)
    ) %>%
    unnest(key = keys)
}