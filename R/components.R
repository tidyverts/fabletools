#' @export
components.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!syms(object%@%"models"))
  kv <- key_vars(object)
  object <- transmute(as_tibble(object),
                      !!!syms(kv), !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  attrs <- combine_dcmp_attr(object[["cmp"]])
  object <- unnest_tsbl(object, "cmp", parent_key = kv)
  as_dable(object, method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

#' @export
components.mdl_ts <- function(object, ...){
  components(object$fit, ...)
}