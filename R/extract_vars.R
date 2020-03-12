#' Extracts the response variable for a model, model table or forecast table
#'
#' This function returns the name of the response variable in a model. For a forecast table, it returns the name of the response variable in the model(s) used to obtain the forecasts.
#'
#' @param x An object of class `mdl_ts`, `mdl_df` or `fbl_ts`.
#' @return
#' A character string.
#' @rdname response_var
#' @export
response_var <- function(x) {
  UseMethod("response_var")
}

#' @rdname response_var
#' @export
response_var.fbl_ts <- function(x) {
  sapply(attr(x, "response"), as.character)
}

#' @rdname response_var
#' @export
response_var.mdl_df <- function(x) {
  mod1 <- mable_models(x)[1]
  response_var(x[[mod1]][[1]])
}

#' @rdname response_var
response_var.mdl_ts <- function(x) {
  sapply(x$response, as.character)
}

#' Extracts the names of the models in a model table
#'
#' @param x An object of class `mdl_df`.
#' @return
#' A character string.
#' @rdname mable_models
#' @export
mable_models <- function(x) {
  UseMethod(x)
}

#' @rdname mable_models
#' @export
mable_models <- function(x) {
  keys <- key_vars(x)
  setdiff(names(x), keys)
}
