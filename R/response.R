#' Extract the response variable from a model
#' 
#' Returns a tsibble containing only the response variable used in the fitting
#' of a model.
#' 
#' @param object The object containing response data
#' @param ... Additional parameters passed on to other methods
#' 
#' @export
response <- function(object, ...){
  UseMethod("response")
}

#' @export
response.mdl_df <- function(object, ...){
  object <- tidyr::pivot_longer(object, all_of(mable_vars(object)),
                             names_to = ".model", values_to = ".fit")
  kv <- c(key_vars(object), ".model")
  object <- transmute(as_tibble(object),
                   !!!syms(kv),
                   !!sym(".model"),
                   response = map(!!sym(".fit"), response)
  )
  unnest_tsbl(object, "response", parent_key = kv)
}

#' @export
response.mdl_ts <- function(object, ...){
  # Extract response
  mv <- measured_vars(object$data)
  resp <- as.list(object$data)[mv]
  
  # Back transform response
  bt <- map(object$transformation, invert_transformation)
  resp <- map2(bt, resp, function(bt, fit) bt(fit))
  
  # Create object
  out <- object$data[index_var(object$data)]
  out[if(length(resp) == 1) ".response" else mv] <- resp
  out
}