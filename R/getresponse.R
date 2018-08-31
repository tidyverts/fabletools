#' @importFrom forecast getResponse
#' @export
getResponse.mable <- function(object, ...){
  object %>%
    transmute(!!!syms(key_vars(.)),
              fitted = map2(!!sym("data"), !!sym("model"),
                            function(data, model) {
                              response <- model%@%"response"
                              data %>% transmute(response = data[[expr_text(attr(model, "response"))]])
                            }
              )
    ) %>%
    unnest(key = syms(key_vars(object)))
}

#' @importFrom forecast getResponse
#' @export
getResponse.fable <- getResponse.mable