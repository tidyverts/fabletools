#' Construct hilo intervals
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param level Default `NULL` does not include 'level'. Otherwise values of
#' length 1 or as length of `lower`, expected between 0 and 100.
#'
#' @return A "hilo" object
#' 
#' @author Earo Wang & Mitchell O'Hara-Wild
#' 
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, level = 95L)
#'
#' @export
new_hilo <- function(lower, upper, level = NULL) {
  if (missing(lower) || missing(upper)) {
    abort("no default for `lower` or `upper`.")
  }
  if(!is.list(lower) && !is.list(upper)){
    lower <- list(lower)
    upper <- list(upper)
  }
  if (any(unlist(upper) < unlist(lower), na.rm = TRUE)) {
    abort("'upper' can't be lower than 'lower'.")
  }
  len <- length(lower[[1]])
  
  if(!is.null(level)){
    if (any(level < 0 | level > 100, na.rm = TRUE)) {
      abort("'level' can't be negative or greater than 100.")
    } else if (!(length(level) %in% c(1, len))) {
      abort(gettextf("'level' should be of length 1 or %d.", len))
    }
  }
  
  list(.lower = transpose_dbl(lower), .upper = transpose_dbl(upper), .level = level) %>% 
    pmap(tibble) %>%
    add_class("hilo")
}

#' Compute hilo intervals
#' 
#' Used to extract a specified prediction interval at a particular confidence 
#' level from a distribution or fable.
#' 
#' @param x Object to create hilo from
#' 
#' @export
hilo <- function(x, ...){
  UseMethod("hilo")
}

#' @export
hilo.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hilo()`, you can create a custom `hilo` with `new_hilo()`",
    class(x)
  ))
}

#' Is the object a hilo
#' 
#' @param x An object.
#' 
#' @export
is_hilo <- function(x) {
  inherits(x, "hilo")
}


#' @export
`$.hilo` <- function(x, name) {
  map_dbl(x, function(.x) .x[[name]])
}

#' @export
`[.hilo` <- function(x, ..., drop = TRUE) {
  add_class(NextMethod(), "hilo")
}

#' @export
c.hilo <- function(...) {
  dots_list(...) %>%
    map(`[`) %>%
    unlist(recursive = FALSE, use.names = FALSE) %>%
    add_class("hilo")
}

#' @export
print.hilo <- function(x, ..., digits = NULL) {
  cat(format(x, digits = digits), sep = "\n")
  invisible(x)
}

#' @export
format.hilo <- function(x, digits = NULL, ...) {
  format(compact_hilo(x, digits = digits), ...)
}

#' @export
is.na.hilo <- function(x) {
  # both lower and upper are NA's
  rowSums(is.na(matrix(c(x$.lower, x$.upper), ncol = 2))) == 2
}

#' @export
duplicated.hilo <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  mat <- matrix(c(x$.lower, x$.upper, x$.level), ncol = 3)
  duplicated(mat, incomparables = incomparables, fromLast = fromLast, ...)
}

#' @export
unique.hilo <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables = incomparables, ...)]
}

#' @export
rep.hilo <- function(x, ...) {
  add_class(NextMethod(), "hilo")
}

type_sum.hilo <- function(x) {
  "hilo"
}

obj_sum.hilo <- function(x) {
  rep("hilo", length(x))
}

is_vector_s3.hilo <- function(x) {
  TRUE
}

pillar_shaft.hilo <- function(x, ...) {
  out <- compact_hilo(x)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 10)
}

underline <- function(...){
  if(requireNamespace("crayon")){
    crayon::underline(...)
  }
  else{
    paste0(..., collapse = " ")
  }
}

compact_hilo <- function(x, digits = NULL) {
  if(NROW(x[[1]]) > 1){
    out <- sprintf("<hilo [%s]>",
      map_chr(map(x, dim), function(x) paste(big_mark(x), collapse = " x ")))
    if(requireNamespace("crayon")){
      out <- crayon::style(out, crayon::make_style("#999999", grey = TRUE))
    }
    return(out)
  }
  limit <- paste(
    format(x$.lower, justify = "right", digits = digits),
    format(x$.upper, justify = "right", digits = digits),
    sep = ", "
  )
  rng <- paste0("[", limit, "]")
  lvl <- x$.level
  if (is.null(lvl)) {
    return(rng)
  } else {
    paste0(rng, underline(lvl))
  }
}

#' @export
as.data.frame.hilo <- function(
  x, row.names = NULL, optional = FALSE, ...,
  nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
) {
  as.data.frame.vector(
    x, row.names = row.names, optional = optional, ...,
    nm = nm
  )
}