#' Identify outliers
#' 
#' Return a table of outlying observations using a fitted model.
#' 
#' @param object An object which can identify outliers.
#' @param ... Arguments for further methods.
#' 
#' @rdname outliers
#' @export
outliers <- function(object, ...){
  UseMethod("outliers")
}

#' @rdname outliers
#' @export
outliers.mdl_df <- function(object, ...){
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  object <- mutate(as_tibble(object), 
                   dplyr::across(all_of(mbl_vars), function(x) lapply(x, outliers, ...)))
  object <- pivot_longer(object, mbl_vars, names_to = ".model", values_to = ".outliers")
  unnest_tsbl(object, ".outliers", parent_key = c(kv, ".model"))
}

#' @rdname outliers
#' @export
outliers.mdl_ts <- function(object, ...){
  object$data[outliers(object$fit, ...),]
}