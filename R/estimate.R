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
  # Clear any leftover lag() memory from a prior estimate() reusing `.model`.
  .model$recent_data <- NULL
  validate_formula(.model, .data)
  parsed <- parse_model(.model)
  
  params <- model_data_params(.data, parsed)
  .data <- model_data(.data, .model, parsed)

  fit <- eval_tidy(
    expr(.model$train(.data = .data, specials = parsed$specials, !!!.model$extra))
  )
  # Store time-varying transformation parameters alongside the response (#382)
  .data[names(params)] <- params
  # Snapshot lag()'s short term memory onto this fit, not just `.model`.
  recent_data <- .model$recent_data
  .model$remove_data()
  .model$stage <- NULL
  new_model(fit, .model, .data, parsed$response, parsed$transformation, recent_data)
}

# The data used to train a model: the index and (transformed) response(s).
# As attributes shouldn't change, using this approach is much faster.
model_data <- function(.data, .model, parsed){
  .dt_attr <- attributes(.data)
  resp <- map(parsed$expressions, eval_tidy, data = .data, env = .model$specials)
  .data <- unclass(.data)[index_var(.data)]
  .data[map_chr(parsed$expressions, expr_name)] <- resp
  attributes(.data) <- c(attributes(.data), .dt_attr[setdiff(names(.dt_attr), names(attributes(.data)))])
  .data
}

# Time-varying transformation parameters from the data
model_data_params <- function(.data, parsed){
  params <- unlist(lapply(parsed$transformation, transformation_params))
  as.list(.data)[setdiff(intersect(params, names(.data)), index_var(.data))]
}
