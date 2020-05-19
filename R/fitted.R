#' Extract fitted values from models
#' 
#' Extracts the fitted values from each of the models in a mable. A tsibble will
#' be returned containing these fitted values. Fitted values will be 
#' automatically back-transformed if a transformation was specified.
#' 
#' @param object A mable or time series model.
#' @param ... Other arguments passed to the model method for `fitted()`
#' 
#' @importFrom stats fitted
#' @export
fitted.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!syms(mable_vars(object)))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
    !!!syms(kv),
    !!sym(".model"),
    fitted = map(!!sym(".fit"), fitted, ...)
  )
  unnest_tsbl(out, "fitted", parent_key = kv)
}

#' @rdname fitted.mdl_df
#' @export
fitted.mdl_ts <- function(object, ...){
  bt <- map(object$transformation, invert_transformation)
  
  fits <- as.matrix(fitted(object$fit, ...))
  fits <- map2(bt, split(fits, col(fits)), function(bt, fit) bt(fit))
  
  nm <- if(length(fits) == 1) ".fitted" else map_chr(object$response, expr_name)
  
  transmute(object$data, !!!set_names(fits, nm))
}