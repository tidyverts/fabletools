#' @importFrom stats residuals
#' @export
residuals.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
    !!!syms(kv),
    !!sym(".model"),
    residuals = map(!!sym(".fit"), residuals, ...)
  )
  unnest(add_class(out, "lst_ts"), key = kv)
}

#' @export
residuals.model <- function(object, type = "innovation", ...){
  if(type == "response"){
    .resid <- response(object)
    .resid <- as.matrix(.resid[measured_vars(.resid)]) - fitted(object$fit)
  }
  else{
    .resid <- residuals(object$fit, type = type, ...)
    if(is.null(.resid)){
        warn(sprintf(
'Residuals of type `%s` are not supported for %s models.
Defaulting to `type="response"`', type, model_sum(object)))
      .resid <- response(object)
      .resid <- as.matrix(.resid[measured_vars(.resid)]) - fitted(object$fit)
    }
  }
  .resid <- as.matrix(.resid)
  
  .resid <- split(.resid, col(.resid))
  nm <- if(length(.resid) == 1) ".resid" else map_chr(object$response, expr_text)
  
  transmute(object$data, !!!set_names(.resid, nm))
}