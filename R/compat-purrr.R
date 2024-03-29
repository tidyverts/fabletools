# nocov start

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on.

# Slightly adapted from the version found in rlang

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
map_mold <- function(...) {
  out <- vapply(..., USE.NAMES = FALSE)
  names(out) <- names(..1)
  out
}
map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

args_recycle <- function(args) {
  vctrs::vec_recycle_common(!!!args)
}
pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    map_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}

compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  inner_names <- names(.l[[1]])

  result <- map(seq_along(.l[[1]]), function(i) {
    map(.l, .subset2, i)
  })
  
  set_names(result, inner_names)
}

transpose_dbl <- function(.l) {
  inner_names <- names(.l[[1]])
  
  result <- map(seq_along(.l[[1]]), function(i) {
    map_dbl(.l, .subset2, i)
  })
  
  set_names(result, inner_names)
}

every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

invoke <- function(.f, .x, ..., .env = NULL){
  .env <- .env %||% parent.frame()
  args <- c(as.list(.x), list(...))
  do.call(.f, args, envir = .env)
}
imap <- function(.x, .f, ...){
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

capture_error <- function (code, otherwise = NULL, quiet = TRUE) 
{
  tryCatch(list(result = code, error = NULL), error = function(e) {
    if (!quiet) 
      message("Error: ", e$message)
    list(result = otherwise, error = e)
  }, interrupt = function(e) {
    stop("Terminated by user", call. = FALSE)
  })
}
safely <- function (.f, otherwise = NULL, quiet = TRUE) 
{
  function(...) capture_error(.f(...), otherwise, quiet)
}
possibly <- function (.f, otherwise, quiet = TRUE) 
{
  force(otherwise)
  function(...) capture_error(.f(...), otherwise, quiet)$result
}
compose <- function (...) {
  fs <- lapply(list(...), match.fun)
  n <- length(fs)
  last <- fs[[n]]
  rest <- fs[-n]
  function(...) {
    out <- last(...)
    for (f in rev(rest)) {
      out <- f(out)
    }
    out
  }
}

flatten <- function(x) {
  unlist(x, recursive = FALSE)
}

squash <- function(x) {
  unlist(x, recursive = TRUE)
}

# nocov end
