#' Create a fable object
#'
#' @inheritParams tsibble::tsibble
#' @param resp The response variable (given as a bare or unquoted variable).
#' @param dist The distribution variable (given as a bare or unquoted variable).
#'
#' @export
fable <- function(..., key = id(), index, resp, dist, regular = TRUE){
  tsbl <- tsibble(..., key = !!enquo(key), index = !!enexpr(index), regular = regular)
  as_fable(tsbl)
}

#' Coerce to a fable object
#' 
#' @inheritParams fable
#' @param x Object to be coerced to a fable (`fbl_ts`)
#' @param ... Additional arguments passed to methods
#' 
#' @rdname as-fable
#' @export
as_fable <- function(x, ...){
  UseMethod("as_fable")
}

#' @rdname as-fable
#' @export
as_fable.tbl_ts <- function(x, resp, dist, ...){
  fbl <- new_tsibble(x, class = "fbl_ts",
                   response = enexpr(resp), dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

validate_fable <- function(fbl){
  stopifnot(inherits(fbl, "fbl_ts"))
  if (!(as_string(fbl%@%"response") %in% names(fbl))){
    abort("Could not find response variable `%s` in the fable.",
          as_string(fbl%@%"response"))
  }
  if (!(as_string(fbl%@%"dist") %in% names(fbl))){
    abort("Could not find distribution variable `%s` in the fable.",
          as_string(fbl%@%"dist"))
  }
  if (!inherits(fbl[[expr_text(fbl%@%"dist")]], "fcdist")){
    abort('Distribution variable must be of class "fcdist"')
  }
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.fbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A fable"
  out
}