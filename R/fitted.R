#' @importFrom stats fitted
#' @export
fitted.mdl_df <- function(object, ...){
  keys <- syms(key_vars(object))
  out <- transmute(object,
    !!!keys,
    fitted = map(!!sym("model"), fitted)
  )
  unnest(add_class(out, "lst_ts"), key = keys)
}
