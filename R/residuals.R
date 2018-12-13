#' @importFrom stats residuals
#' @export
residuals.mdl_df <- function(object, ...){
  keys <- syms(key_vars(object))
  
  out <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  out <- transmute(out,
                   !!!keys,
                   !!sym(".model"),
                   residuals = map(!!sym(".fit"), residuals)
  )
  unnest(add_class(out, "lst_ts"), key = keys)
}

#' @export
residuals.model <- function(object, ...){
  residuals(object$fit, ...)
}