#' Estimate a model
#' 
#' @param .data A data structure suitable for the models (such as a `tsibble`)
#' @param .model Definition for the model to be used
#' 
#' @export
estimate <- function(.data, .model){
  UseMethod("estimate")
}

#' @export
estimate.tbl_ts <- function(.data, .model){
  .model$add_data(.data)
  validate_formula(.model, .data)
  parsed <- parse_model(.model)
  if(length(parsed$response) > 1){
    abort("Multivariate modelling is not yet supported")
  }
  else{
    parsed$response <- parsed$response[[1]]
    parsed$transformation <- parsed$transformation[[1]]
  }
  data <- eval_tidy(expr(transmute(data, !!!parsed$expressions)),
                    env = env_bury(.model$env, data = .data, transmute = transmute))
  fit <- eval_tidy(
    expr(.model$train(.data = .data, formula = .model$formula,
                     specials = parsed$specials, !!!.model$extra))
  )
  .model$remove_data()
  new_model(fit, .model, parsed$response, parsed$transformation)
}
