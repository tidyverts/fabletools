#' @export
components.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(object)
  object <- transmute(object, !!!keys, !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  unnest(add_class(object, "lst_ts"), key = keys)
}

#' @export
components.model <- function(object, ...){
  components(object$fit, ...)
}