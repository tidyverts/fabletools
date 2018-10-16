#' @importFrom stats fitted
#' @export
fitted.mable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              fitted = map(!!sym("model"), fitted)
    ) %>%
    unnest(key = keys)
}
