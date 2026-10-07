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
  is_mdl <- map_lgl(cols, inherits, c("lst_mdl", "mdl_lst", "mdl_df"))
  # val_key <- any(key_vars(data) %in% cols)
  # if (val_key) {
  #   key_vars <- setdiff(names(res), measured_vars(data))
  #   data <- remove_key(data, key_vars)
  # }
  build_mable(res, 
              key = !!key_vars(data), 
              model = union(mable_vars(data), names(which(is_mdl))),
              template = data)
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
