#' Estimate a model
#' 
#' @param .data A data structure suitable for the models (such as a `tsibble`).
#' @param ... Further arguments passed to methods.
#' 
#' @rdname estimate
#' 
#' @export
estimate <- function(.data, ...){
  UseMethod("estimate")
}

#' @param .model Definition for the model to be used.
#' 
#' @rdname estimate
#' @export
estimate.tbl_ts <- function(.data, .model, ...){
  if(!inherits(.model, "mdl_defn")){
    abort("Model definition incorrectly created. Check that specified model(s) are model definitions.")
  }
  .model$stage <- "estimate"
  .model$add_data(.data)
  validate_formula(.model, .data)
  parsed <- parse_model(.model)
  .data <- eval_tidy(expr(transmute(!!sym("data"), !!!parsed$expressions)),
                    env = env_bury(.model$env, data = .data, transmute = transmute))
  fit <- eval_tidy(
    expr(.model$train(.data = .data, formula = .model$formula,
                     specials = parsed$specials, !!!.model$extra))
  )
  .model$remove_data()
  .model$stage <- NULL
  new_model(fit, .model, .data, parsed$response, parsed$transformation)
}
