#' @importFrom stats residuals
#' @export
residuals.mable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              residuals = map2(!!sym("data"), !!sym("model"),
                               function(data, model) {
                                 data %>% transmute(residuals = data[[expr_text((model%@%"fable")$response)]] - as.numeric(fitted(model)))
                               }
              )
    )%>%
    unnest(key = keys)
}


#' @importFrom stats residuals
#' @export
residuals.fable <- residuals.mable
