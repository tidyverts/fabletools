#' @importFrom forecast getResponse
#' @export
getResponse.mable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(!!!keys,
              fitted = map2(!!sym("data"), !!sym("model"),
                            function(data, model) {
                              response <- model%@%"response"
                              data %>% transmute(response = data[[expr_text(attr(model, "response"))]])
                            }
              )
    ) %>%
    unnest(key = keys)
}

#' @importFrom forecast getResponse
#' @export
getResponse.fable <- getResponse.mable