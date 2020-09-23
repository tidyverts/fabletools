#' Internal vctrs methods
#'
#' These methods are the extensions that allow dable objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name dable-vctrs
NULL

#' @rdname dable-vctrs
#' @export
vec_ptype2.dcmp_ts <- function(x, y, ...) {
  UseMethod("vec_ptype2.dcmp_ts", y)
}

#' @export
vec_ptype2.dcmp_ts.dcmp_ts <- function(x, y, ...) {
  dable_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.dcmp_ts <- function(x, y, ...) {
  dable_ptype2(y, x, ...)
}

#' @export
vec_ptype2.dcmp_ts.data.frame <- vec_ptype2.dcmp_ts.dcmp_ts

#' @export
vec_ptype2.tbl_df.dcmp_ts <- vec_ptype2.data.frame.dcmp_ts

#' @export
vec_ptype2.dcmp_ts.tbl_df <- vec_ptype2.dcmp_ts.dcmp_ts

dable_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  seas_x <- x%@%"seasons"
  alias_x <- x%@%"aliases"
  method_x <- x%@%"method"
  resp_x <- response_vars(x)
  if (is_dable(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    if (!identical(resp_x, response_vars(y))) {
      abort("Objects with different response variables cannot be combined.")
    }
    if(method_x != y%@%"method") method_x <- NULL
    seas_x <- vec_unique(c(seas_x, y%@%"seasons"))
    alias_x <- vec_unique(c(alias_x, y%@%"aliases"))
    key_x <- union(key_x, key_vars(y))
  }
  out <- df_ptype2(x, y, ...)
  tsbl <- build_tsibble_meta(
    out, key_data = tibble(!!!x[key_x], !!".rows" := list_of(.ptype = integer())),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
  build_dable(tsbl, response = resp_x, method = method_x, 
              seasons = seas_x, aliases = alias_x)
}

#' @rdname dable-vctrs
#' @export
vec_cast.dcmp_ts <- function(x, to, ...) {
  UseMethod("vec_cast.dcmp_ts")
}

#' @export
vec_cast.dcmp_ts.dcmp_ts <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to),
    key_data = if (is_identical) key_data(x) else NULL,
    index = index_var(to), index2 = index2_var(to),
    ordered = is_ordered(to),
    validate = FALSE, .drop = key_drop_default(to))
  build_dable(tsbl, response = response_vars(to), method = to%@%"method", 
              seasons = to%@%"seasons", aliases = to%@%"aliases")
}

#' @export
vec_cast.dcmp_ts.tbl_df <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to))
  build_dable(tsbl, response = response_vars(to), method = to%@%"method", 
              seasons = to%@%"seasons", aliases = to%@%"aliases")
}

#' @export
vec_cast.dcmp_ts.data.frame <- vec_cast.dcmp_ts.tbl_df

#' @export
vec_cast.tbl_df.dcmp_ts <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.dcmp_ts <- function(x, to, ...) {
  df_cast(x, to, ...)
}

#' @export
vec_cast.tbl_ts.dcmp_ts <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  build_tsibble(
    tbl, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to)
  )
}