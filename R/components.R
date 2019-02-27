#' @export
components.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(object)
  object <- transmute(as_tibble(object),
                      !!!keys, !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  attrs <- combine_dcmp_attr(object[["cmp"]])
  object <- unnest(add_class(object, "lst_ts"), key = keys)
  as_dable(object, method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

#' @export
components.model <- function(object, ...){
  components(object$fit, ...)
}