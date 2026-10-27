#' @export
dplyr_row_slice.mbl_df <- function(data, i, ..., preserve = FALSE) {
  res <- dplyr_row_slice(as_tibble(data), i, ..., preserve = preserve)
  build_mable_meta(
    res,
    key_data = dplyr::group_data(dplyr::group_by(res, !!!syms(key_vars(data)))),
    model = mable_vars(data),
    response = response_vars(data),
    ptype = if(NROW(res) == 0) mable_ptype(data)
  )
}

#' @export
dplyr_col_modify.mbl_df <- function(data, cols) {
  res <- dplyr_col_modify(as_tibble(data), cols)
  restore_mable_assign(res, data)
}

#' @export
dplyr_reconstruct.mbl_df <- function(data, template) {
  res <- NextMethod()
  mbl_vars <- names(which(vapply(data, inherits, logical(1L), c("mdl_lst", "mdl_df"))))
  kv <- key_vars(template)
  if(all(kv %in% names(res))) {
    build_mable(data, key = !!kv, model = mbl_vars, template = template)
  } else {
    as_tibble(res)
  }
}
