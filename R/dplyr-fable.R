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
  dist <- distribution_var(template)
  if(dist %in% names(res)) {
    build_fable(res, response = response_vars(template), distribution = dist)
  } else {
    res
  }
}

#' @export
dplyr_reconstruct.grouped_fbl <- dplyr_reconstruct.fbl_ts

#' @export
summarise.fbl_ts <- function(.data, ..., .groups = NULL) { 
  dist_var <- distribution_var(.data)
  dist_ptype <- vec_ptype(.data[[dist_var]])
  resp_var <- response_vars(.data)
  .data <- summarise(as_tsibble(.data), ..., .groups = .groups)
  
  # If the distribution is lost, return a tsibble
  if(!(dist_var %in% names(.data))) {
    if(!vec_is(.data[[dist_var]], dist_ptype)){
      return(.data)
    }
  }
  
  build_fable(.data, response = resp_var, distribution = dist_var)
}

#' @export
summarise.grouped_fbl <- summarise.fbl_ts