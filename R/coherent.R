#' Internal vctrs methods
#'
#' These methods are the extensions that allow coherent_tsibble objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name coherent_tsibble-vctrs
NULL

#' @rdname coherent_tsibble-vctrs
#' @method vec_ptype2 str_tbl_ts
#' @export
vec_ptype2.str_tbl_ts <- function(x, y, ...) {
  UseMethod("vec_ptype2.str_tbl_ts", y)
}

#' @export
vec_ptype2.str_tbl_ts.str_tbl_ts <- function(x, y, ...) {
  coherent_tsibble_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.str_tbl_ts <- function(x, y, ...) {
  coherent_tsibble_ptype2(y, x, ...)
}

#' @export
vec_ptype2.str_tbl_ts.data.frame <- vec_ptype2.str_tbl_ts.str_tbl_ts

#' @export
vec_ptype2.tbl_df.str_tbl_ts <- vec_ptype2.data.frame.str_tbl_ts

#' @export
vec_ptype2.str_tbl_ts.tbl_df <- vec_ptype2.str_tbl_ts.str_tbl_ts

coherent_tsibble_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  struct_x <- struct_data(x)
  if (is_coherent_tsibble(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    key_x <- union(key_x, key_vars(y))
  }
  out <- df_ptype2(x, y, ...)
  tsbl <- build_tsibble_meta(
    out, key_data = tibble(!!!x[key_x], !!".rows" := list_of(.ptype = integer())),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
  build_coherent_tsibble(tsbl, structure = struct_x)
}

#' @rdname coherent_tsibble-vctrs
#' @method vec_cast str_tbl_ts
#' @export
vec_cast.str_tbl_ts <- function(x, to, ...) {
  UseMethod("vec_cast.str_tbl_ts")
}

#' @export
vec_cast.str_tbl_ts.str_tbl_ts <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to),
    key_data = if (is_identical) key_data(x) else NULL,
    index = index_var(to), index2 = index2_var(to),
    ordered = is_ordered(to),
    validate = FALSE, .drop = key_drop_default(to))
  build_coherent_tsibble(tsbl, structure = struct_data(to))
}

#' @export
vec_cast.str_tbl_ts.tbl_df <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  tsbl <- build_tsibble(
    tbl, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to))
  build_coherent_tsibble(tsbl, structure = struct_data(to))
}

#' @export
vec_cast.tbl_ts.str_tbl_ts <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  build_tsibble(
    tbl, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to)
  )
}

#' @export
vec_cast.str_tbl_ts.data.frame <- vec_cast.str_tbl_ts.tbl_df

#' @export
vec_cast.tbl_df.str_tbl_ts <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.str_tbl_ts <- function(x, to, ...) {
  df_cast(x, to, ...)
}

#' @export
vec_restore.str_tbl_ts <- function(x, to, ..., n = NULL) {
  if(!is_tsibble(x)){
    x <- build_tsibble(
      x, key = key_vars(to), index = index_var(to), index2 = index2_var(to),
      ordered = TRUE, validate = TRUE, .drop = key_drop_default(to))
  }
  build_coherent_tsibble(tsbl, structure = struct_data(to))
}


#' Create a coherent_tsibble object
#'
#' A coherent_tsibble (forecast table) data class (`str_tbl_ts`) which is a tsibble-like data
#' structure for representing forecasts. In extension to the key and index from 
#' the tsibble (`tbl_ts`) class, a coherent_tsibble (`str_tbl_ts`) must also contain a single
#' distribution column that uses values from the distributional package.
#'
#' @param ... Arguments passed to [tsibble::tsibble()].
#' @param response The character vector of response variable(s).
#' @param distribution The name of the distribution column (can be provided
#' using a bare expression).
#'
#' @export
coherent_tsibble <- function(..., structure){
  build_coherent_tsibble(tsibble(...), structure)
}

#' Is the object a coherent_tsibble
#' 
#' @param x An object.
#' 
#' @export
is_coherent_tsibble <- function(x){
  inherits(x, "str_tbl_ts")
}

#' Coerce to a coherent_tsibble object
#' 
#' @inheritParams coherent_tsibble
#' @param x Object to be coerced to a coherent_tsibble (`str_tbl_ts`)
#' @param ... Additional arguments passed to methods
#' 
#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble <- function(x, ...){
  UseMethod("as_coherent_tsibble")
}

#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble.tbl_ts <- function(x, structure, ...){
  build_coherent_tsibble(x, structure)
}

#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble.grouped_ts <- as_coherent_tsibble.tbl_ts

#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble.tbl_df <- function(x, ...){
  build_coherent_tsibble(as_tsibble(x, ...), structure)
}

#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble.str_tbl_ts <- function(x, structure, ...){
  build_coherent_tsibble(update_tsibble(x, ...), structure)
}

#' @rdname as-coherent_tsibble
#' @export
as_coherent_tsibble.grouped_df <- as_coherent_tsibble.tbl_df

build_coherent_tsibble <- function (x, structure) {
  if(is_grouped_ts(x)){
    str_ts <- structure(x, class = c("grouped_str_ts", "grouped_ts", "grouped_df", 
                                  "str_tbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                     structure = structure,
                     model_cn = ".model")
  } else {
    str_ts <- tsibble::new_tsibble(
      x, structure = structure, model_cn = ".model",
      class = "str_tbl_ts")
  }
  validate_coherent_tsibble(str_ts)
  str_ts
}


#' @export
struct_data <- function (.data) {
  UseMethod("struct_data")
}

#' @export
struct_data.str_tbl_ts <- function(.data) {
  attr(.data, "structure")
}

#' @export
as_tsibble.str_tbl_ts <- function(x, ...){
  new_tsibble(x)
}

#' @export
as_tsibble.grouped_str_ts <- function(x, ...){
  structure(x, class=setdiff(class(x), c("grouped_str_ts", "str_tbl_ts")),
            structure = NULL)
}

#' @export
as_tibble.str_tbl_ts <- function(x, ...) {
  new_tibble(vec_data(x), nrow = nrow(x))
}

#' @export
as_tibble.grouped_str_ts <- function(x, ...) {
  dplyr::new_grouped_df(as_tibble(vec_data(x)), groups = group_data(x))
}

validate_coherent_tsibble <- function(str_ts){
  stopifnot(inherits(str_ts, "str_tbl_ts"))
}

tbl_sum.str_tbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A coherent tsibble"
  out
}

#' @importFrom distributional hilo
#' @export
hilo.str_tbl_ts <- function(x, level = c(80, 95), ...){
  hilo_exprs <- map(level,function(.x) expr(hilo(!!sym(distribution_var(x)), !!.x)))
  names(hilo_exprs) <- paste0(level, "%")
  as_tsibble(x) %>%
    mutate(!!!hilo_exprs)
}

restore_coherent_tsibble <- function(data, template){
  build_coherent_tsibble(data, structure = struct_data(template))
}

#' @export
select.str_tbl_ts <- function (.data, ...){
  res <- select(as_tsibble(.data), ...)
  restore_coherent_tsibble(res, .data)
}

#' @export
select.grouped_str_ts <- select.str_tbl_ts

#' @export
transmute.str_tbl_ts <- function (.data, ...) {
  res <- transmute(as_tsibble(.data), ...)
  restore_coherent_tsibble(res, .data)
}

#' @export
transmute.grouped_str_ts <- transmute.str_tbl_ts

#' @export
group_by.str_tbl_ts <- function(.data, ...) {
  build_coherent_tsibble(NextMethod(), struct_data(.data))
}

#' @export
group_by.grouped_str_ts <- group_by.str_tbl_ts

#' @export
ungroup.str_tbl_ts <- function(x, ...) {
  build_coherent_tsibble(NextMethod())
}

#' @export
ungroup.grouped_str_ts <- function(x, ...) {
  build_coherent_tsibble(NextMethod())
}

#' @export
fill_gaps.str_tbl_ts <- function(.data, ..., .full = FALSE) {
  vec_restore(NextMethod(.data), .data)
}

#' @export
`[.str_tbl_ts` <- function (x, i, j, drop = FALSE){
  out <- NextMethod()
  # Drop coherent_tsibble if tsibble is dropped
  
  cn <- colnames(out)
  not_coherent_tsibble <- !(distribution_var(x) %in% cn) || !is_tsibble(out)
  
  if(not_coherent_tsibble)
    return(out)
  else
    build_coherent_tsibble(out, struct_data(x))
}

type_sum.str_tbl_ts <- function(x){
  "tsibble*"
}