#' Internal vctrs methods
#'
#' These methods are the extensions that allow fable objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name fable-vctrs
NULL

#' @rdname fable-vctrs
#' @export
vec_ptype2.fbl_ts <- function(x, y, ...) {
  UseMethod("vec_ptype2.fbl_ts", y)
}

#' @export
vec_ptype2.fbl_ts.fbl_ts <- function(x, y, ...) {
  fable_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.fbl_ts <- function(x, y, ...) {
  fable_ptype2(y, x, ...)
}

#' @export
vec_ptype2.fbl_ts.data.frame <- vec_ptype2.fbl_ts.fbl_ts

#' @export
vec_ptype2.tbl_df.fbl_ts <- vec_ptype2.data.frame.fbl_ts

#' @export
vec_ptype2.fbl_ts.tbl_df <- vec_ptype2.fbl_ts.fbl_ts

fable_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  dist_x <- distribution_var(x)
  resp_x <- response_vars(x)
  if (is_fable(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    if (dist_x != distribution_var(y)) {
      abort("No common distribution variable for `x` and `y`.")
    }
    if (!identical(resp_x, response_vars(y))) {
      abort("Objects with different response variables cannot be combined.")
    }
    key_x <- union(key_x, key_vars(y))
  }
  out <- df_ptype2(x, y, ...)
  tsbl <- build_tsibble_meta(
    out, key_data = tibble(!!!x[key_x], !!".rows" := list_of(.ptype = integer())),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
  build_fable(tsbl, response = resp_x, distribution = !!dist_x)
}

#' @rdname fable-vctrs
#' @export
vec_cast.fbl_ts <- function(x, to, ...) {
  UseMethod("vec_cast.fbl_ts")
}

#' @export
vec_cast.fbl_ts.fbl_ts <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to),
    key_data = if (is_identical) key_data(x) else NULL,
    index = index_var(to), index2 = index2_var(to),
    ordered = is_ordered(to),
    validate = FALSE, .drop = key_drop_default(to))
  build_fable(tsbl, response = response_vars(to), distribution = distribution_var(to))
}

#' @export
vec_cast.fbl_ts.tbl_df <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to))
  build_fable(tsbl, response = response_vars(to), distribution = distribution_var(to))
}

#' @export
vec_cast.fbl_ts.data.frame <- vec_cast.fbl_ts.tbl_df

#' @export
vec_cast.tbl_df.fbl_ts <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.fbl_ts <- function(x, to, ...) {
  df_cast(x, to, ...)
}
