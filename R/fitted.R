#' @importFrom stats fitted
#' @export
fitted.mable <- function(object, ...){
  object %>%
    transmute(!!!syms(key_vars(.)),
              fitted = map2(!!sym("data"), !!sym("model"),
                            function(data, model) {
                              data %>% transmute(fitted = as.numeric(fitted(model)))
                            }
              )
    ) %>%
    unnest(key = syms(key_vars(object)))
}

#' @importFrom stats fitted
#' @export
fitted.fable <- fitted.mable