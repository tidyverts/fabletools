#' @importFrom stats fitted
#' @export
fitted.mable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              fitted = map2(!!sym("data"), !!sym("model"),
                            function(data, model) {
                              data %>% transmute(fitted = as.numeric(fitted(model)))
                            }
              )
    ) %>%
    unnest(key = keys)
}

#' @importFrom stats fitted
#' @export
fitted.fable <- fitted.mable