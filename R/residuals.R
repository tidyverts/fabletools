#' @importFrom stats residuals
#' @export
residuals.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(out)
  out <- transmute(as_tibble(out),
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