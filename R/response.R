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
  out <- gather(object, ".model", ".fit", !!!syms(object%@%"models"))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
                   !!!syms(kv),
                   !!sym(".model"),
                   response = map(!!sym(".fit"), response)
  )
  unnest_tsbl(out, "response", parent_key = kv)
}

#' @export
response.mdl_ts <- function(object, ...){
  bt <- map(object$transformation, invert_transformation)
  
  resp <- as.list(object$data)[measured_vars(object$data)]
  resp <- map2(bt, resp, function(bt, fit) bt(fit))
  
  nm <- if(length(resp) == 1) ".response" else map_chr(object$response, expr_text)
  
  transmute(object$data, !!!set_names(resp, nm))
}