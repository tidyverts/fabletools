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
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  object <- mutate(as_tibble(object), 
              dplyr::across(all_of(mbl_vars), function(x) lapply(x, residuals, ...)))
  object <- pivot_longer(object, mbl_vars, names_to = ".model", values_to = ".resid")
  unnest_tsbl(object, ".resid", parent_key = c(kv, ".model"))
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
      if(type == "innovation") {
        .resid <- response(object)
        .resid <- map2(object$transformation, .resid[measured_vars(.resid)], calc)
        .fits <- fitted(object)
        .fits <- map2(object$transformation, .fits[measured_vars(.fits)], calc)
        .resid <- do.call(cbind, .resid) - do.call(cbind, as.matrix(.fits))
      } else {
        warn(sprintf(
'Residuals of type `%s` are not supported for %s models.
Defaulting to `type="response"`', type, model_sum(object)))
        return(residuals(object, type = "response", ...))
      }
    }
  }
  .resid <- as.matrix(.resid)
  
  .resid <- split(.resid, col(.resid))
  nm <- if(length(.resid) == 1) ".resid" else map_chr(object$response, expr_name)
  
  out <- object$data[index_var(object$data)]
  out[nm] <- .resid
  out
}