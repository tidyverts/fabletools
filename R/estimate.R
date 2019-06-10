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
  
  # Compute response data (as attributes shouldn't change, using this approach should be much faster)
  .dt_attr <- attributes(.data)
  resp <- map(parsed$expressions, eval_tidy, data = .data, env = .model$specials)
  .data <- unclass(.data)[expr_text(index(.data))]
  .data[map_chr(parsed$expressions, expr_text)] <- resp
  attributes(.data) <- c(attributes(.data), .dt_attr[setdiff(names(.dt_attr), names(attributes(.data)))])
  
  fit <- eval_tidy(
    expr(.model$train(.data = .data, specials = parsed$specials, !!!.model$extra))
  )
  .model$remove_data()
  .model$stage <- NULL
  new_model(fit, .model, .data, parsed$response, parsed$transformation)
}
