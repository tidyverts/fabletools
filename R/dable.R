#' Create a dable object
#'
#' @inheritParams tsibble::tsibble
#' @inheritParams fable
#' @param dcmp An expression of data columns defining how the variable was decomposed.
#'
#' @export
dable <- function(..., key = id(), index, resp, dcmp, regular = TRUE){
  tsbl <- tsibble(..., key = !!enquo(key), index = !!enexpr(index), regular = regular)
  as_dable(tsbl, !!enexpr(resp), !!enexpr(dcmp))
}

#' Coerce to a dable object
#' 
#' @inheritParams as_fable
#' @param x Object to be coerced to a dable (`dcmp_ts`)
#' 
#' @rdname as-dable
#' @export
as_dable <- function(x, ...){
  UseMethod("as_dable")
}

#' @rdname as-dable
#' 
#' @inheritParams dable
#' 
#' @export
as_dable.tbl_ts <- function(x, resp, dcmp, ...){
  fbl <- as_tsibble(x, class = "dcmp_ts",
                   resp = enexpr(resp),
                   dcmp = enexpr(dcmp))
  fbl
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.dcmp_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A dable"
  append(out,
         c("Decomposition" = 
             paste(expr_text(x%@%"resp"), expr_text(x%@%"dcmp"), sep = " = ")))
}