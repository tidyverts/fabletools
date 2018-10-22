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
#' 
#' @rdname as-fable
#' @export
as_fable <- function(x, ...){
  UseMethod("as_fable")
}

#' @rdname as-fable
#' @export
as_fable.tbl_ts <- function(x, resp, dist, ...){
  fbl <- structure(x, class = c("fbl_ts", class(x)),
                   response = enexpr(resp), dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

validate_fable <- function(fbl){
  stopifnot(inherits(fbl, "fbl_ts"))
  if (!(expr_text(response(fbl)) %in% names(fbl))){
    abort("Could not find response variable `%s` in the fable.",
          expr_text(response(fbl)))
  }
  if (!(expr_text(fbl%@%"dist") %in% names(fbl))){
    abort("Could not find distribution variable `%s` in the fable.",
          expr_text(fbl%@%"dist"))
  }
  if (!inherits(fbl[[expr_text(fbl%@%"dist")]], "fcdist")){
    abort('Distribution variable must be of class "fcdist"')
  }
}

#' @export
response.fbl_ts <- function(x, ...){
  x%@%response
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.fbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A fable"
  out
}

#' @export
summary.fbl_ts <- function(object, level=c(80,95), ...){
  object %>%
    transmute(
      !!response(object),
      !!!set_names(map(level,function(.x) expr(hilo(!!object%@%"dist", !!.x))),
                   paste0(level, "%")))
}