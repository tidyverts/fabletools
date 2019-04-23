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
    .resid <- response(object)[[".response"]] - fitted(object$fit)
  }
  else{
    .resid <- residuals(object$fit, type = type, ...)
    if(is.null(.resid)){
        warn(sprintf(
'Residuals of type `%s` are not supported for %s models.
Defaulting to `type="response"`', type, model_sum(object)))
        .resid <- response(object)[[".response"]] - fitted(object$fit)
    }
  }
  transmute(object$data, .resid = .resid)
}