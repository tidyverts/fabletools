#' @importFrom stats residuals
#' @export
residuals.mable <- function(object, ...){
  object %>%
    transmute(!!!syms(key_vars(.)),
              residuals = map2(!!sym("data"), !!sym("model"),
                               function(data, model) {
                                 data %>% transmute(residuals = data[[expr_text((model%@%"fable")$response)]] - as.numeric(fitted(model)))
                               }
              )
    )%>%
    unnest(key = id(key_vars(object)))
}


#' @importFrom stats residuals
#' @export
residuals.fable <- residuals.mable
