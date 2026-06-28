#' @export
dplyr_row_slice.mdl_df <- function(data, i, ..., preserve = FALSE) {
  res <- dplyr_row_slice(as_tibble(data), i, ..., preserve = preserve)
  build_mable_meta(
    res,
    key_data = dplyr::group_data(dplyr::group_by(res, !!!syms(key_vars(data)))),
    model = mable_vars(data),
    response = response_vars(data)
  )
}

#' @export
dplyr_col_modify.mdl_df <- function(data, cols) {
  res <- dplyr_col_modify(as_tibble(data), cols)
  is_mdl <- map_lgl(cols, inherits, c("lst_mdl", "mdl_lst"))
  # val_key <- any(key_vars(data) %in% cols)
  # if (val_key) {
  #   key_vars <- setdiff(names(res), measured_vars(data))
  #   data <- remove_key(data, key_vars)
  # }
  build_mable(res, 
              key = !!key_vars(data), 
              model = union(mable_vars(data), names(which(is_mdl))))
}

#' @export
dplyr_reconstruct.mdl_df <- function(data, template) {
  res <- NextMethod()
  mbl_vars <- names(which(vapply(data, inherits, logical(1L), "mdl_lst")))
  kv <- key_vars(template)
  if(all(kv %in% names(res))) {
    build_mable(data, key = !!kv, model = mbl_vars)
  } else {
    as_tibble(res)
  }
}
