#' @importFrom stats fitted
#' @export
fitted.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!syms(object%@%"models"))
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
    !!!syms(kv),
    !!sym(".model"),
    fitted = map(!!sym(".fit"), fitted, ...)
  )
  unnest_tsbl(out, "fitted", parent_key = kv)
}

#' @export
fitted.mdl_ts <- function(object, ...){
  bt <- map(object$transformation, invert_transformation)
  
  fits <- as.matrix(fitted(object$fit, ...))
  fits <- map2(bt, split(fits, col(fits)), function(bt, fit) bt(fit))
  
  nm <- if(length(fits) == 1) ".fitted" else map_chr(object$response, expr_text)
  
  transmute(object$data, !!!set_names(fits, nm))
}