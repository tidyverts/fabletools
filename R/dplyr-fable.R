#' @export
dplyr_row_slice.fbl_ts <- function(data, i, ..., preserve = FALSE) {
  res <- NextMethod()
  build_fable(res, response = response_vars(data), distribution = distribution_var(data))
}

#' @export
dplyr_row_slice.grouped_fbl <- dplyr_row_slice.fbl_ts

#' @export
dplyr_col_modify.fbl_ts <- function(data, cols) {
  res <- NextMethod()
  build_fable(res, response = response_vars(data), distribution = distribution_var(data))
}

#' @export
dplyr_col_modify.grouped_fbl <- dplyr_col_modify.fbl_ts

#' @export
dplyr_reconstruct.fbl_ts <- function(data, template) {
  res <- NextMethod()
  build_fable(res, response = response_vars(template), distribution = distribution_var(template))
}

#' @export
dplyr_reconstruct.grouped_fbl <- dplyr_reconstruct.fbl_ts
