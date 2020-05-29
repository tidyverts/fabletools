#' @export
dplyr_row_slice.dcmp_ts <- function(data, i, ..., preserve = FALSE) {
  res <- NextMethod()
  build_dable(res, response = response_vars(data), method = data%@%"method", 
              seasons = data%@%"seasons", aliases = data%@%"aliases")
}

#' @export
dplyr_col_modify.dcmp_ts <- function(data, cols) {
  res <- NextMethod()
  build_dable(res, response = response_vars(data), method = data%@%"method", 
              seasons = data%@%"seasons", aliases = data%@%"aliases")
}

#' @export
dplyr_reconstruct.dcmp_ts <- function(data, template) {
  res <- NextMethod()
  build_dable(res, response = response_vars(template), method = template%@%"method", 
              seasons = template%@%"seasons", aliases = template%@%"aliases")
}
