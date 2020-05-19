#' Extract residuals values from models
#' 
#' Extracts the residuals from each of the models in a mable. A tsibble will
#' be returned containing these residuals.
#' 
#' @param object A mable or time series model.
#' @param ... Other arguments passed to the model method for `residuals()`
#' 
#' @importFrom stats residuals
#' @export
residuals.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!syms(mable_vars(object)))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
    !!!syms(kv),
    !!sym(".model"),
    residuals = map(!!sym(".fit"), residuals, ...)
  )
  unnest_tsbl(out, "residuals", parent_key = kv)
}

#' @param type The type of residuals to compute. If `type="response"`, residuals on the back-transformed data will be computed.
#' @rdname residuals.mdl_df
#' @export
residuals.mdl_ts <- function(object, type = "innovation", ...){
  if(type == "response"){
    .resid <- response(object)
    .fits <- fitted(object)
    .resid <- as.matrix(.resid[measured_vars(.resid)]) - as.matrix(.fits[measured_vars(.fits)])
  }
  else{
    .resid <- residuals(object$fit, type = type, ...)
    if(is.null(.resid)){
        warn(sprintf(
'Residuals of type `%s` are not supported for %s models.
Defaulting to `type="response"`', type, model_sum(object)))
      .resid <- response(object)
      .fits <- fitted(object)
      .resid <- as.matrix(.resid[measured_vars(.resid)]) - as.matrix(.fits[measured_vars(.fits)])
    }
  }
  .resid <- as.matrix(.resid)
  
  .resid <- split(.resid, col(.resid))
  nm <- if(length(.resid) == 1) ".resid" else map_chr(object$response, expr_name)
  
  transmute(object$data, !!!set_names(.resid, nm))
}