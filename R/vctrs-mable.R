#' Internal vctrs methods
#'
#' These methods are the extensions that allow mable objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name mable-vctrs
NULL

#' @rdname mable-vctrs
#' @export
vec_ptype2.mdl_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.mdl_df", y)
}

#' @export
vec_ptype2.mdl_df.mdl_df <- function(x, y, ...) {
  mable_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.mdl_df <- function(x, y, ...) {
  mable_ptype2(y, x, ...)
}

#' @export
vec_ptype2.mdl_df.data.frame <- vec_ptype2.mdl_df.mdl_df

#' @export
vec_ptype2.tbl_df.mdl_df <- vec_ptype2.data.frame.mdl_df

#' @export
vec_ptype2.mdl_df.tbl_df <- vec_ptype2.mdl_df.mdl_df

mable_ptype2 <- function(x, y, ...) {
  key_x <- key_vars(x)
  mdl_x <- mable_vars(x)
  if (is_mable(y)) {
    resp_x <- response_vars(x)
    if (!identical(resp_x, response_vars(y))) {
      abort("Objects with different response variables cannot be combined.")
    }
    key_x <- union(key_x, key_vars(y))
    mdl_x <- union(mdl_x, mable_vars(y))
  }
  out <- df_ptype2(x, y, ...)
  build_mable_meta(out, key_data = key_x, model = mdl_x)
}

#' @rdname mable-vctrs
#' @export
vec_cast.mdl_df <- function(x, to, ...) {
  UseMethod("vec_cast.mdl_df")
}

#' @export
vec_cast.mdl_df.mdl_df <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- tib_cast(x, to, ...)
  build_mable(tbl,
              key = !!key_vars(to), 
              key_data = if (is_identical) key_data(x) else NULL,
              model = mable_vars(to))
}

#' @export
vec_cast.mdl_df.tbl_df <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  build_mable(tbl,
              key = !!key_vars(to), 
              key_data = NULL,
              model = mable_vars(to))
}

#' @export
vec_cast.mdl_df.data.frame <- vec_cast.mdl_df.tbl_df

#' @export
vec_cast.tbl_df.mdl_df <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.mdl_df <- function(x, to, ...) {
  df_cast(x, to, ...)
}
