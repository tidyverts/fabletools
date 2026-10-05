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
response.mbl_df <- mbl_df_method(response, ".response", unnest = "tsibble")

#' @export
response.mdl_df <- mdl_df_method(response)

#' @export
response.mdl_lst <- mdl_lst_method(response)

#' @export
response.mdl_ts <- function(object, ...){
  # Extract response
  mv <- model_response_cols(object)
  resp <- as.list(object$data)[mv]
  
  # Back transform response
  bt <- map(object$transformation, function(x) {
    invert_transformation(bind_transformation_data(x, object$data))
  })
  resp <- map2(bt, resp, function(bt, fit) bt(fit))
  
  # Create object
  out <- object$data[index_var(object$data)]
  out[if(length(resp) == 1) ".response" else mv] <- resp
  out
}