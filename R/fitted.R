#' @importFrom stats fitted
#' @export
fitted.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
    !!!syms(kv),
    !!sym(".model"),
    fitted = map(!!sym(".fit"), fitted, ...)
  )
  unnest(add_class(out, "lst_ts"), key = kv)
}

#' @export
fitted.model <- function(object, ...){
  bt <- invert_transformation(object$transformation)
  mutate(object$index, .fitted = bt(fitted(object$fit, ...)))
}