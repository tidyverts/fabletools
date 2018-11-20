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
  fbl <- new_tsibble(x, class = "dcmp_ts",
                   resp = enexpr(resp),
                   dcmp = enexpr(dcmp))
  fbl
}

#' @export
as_tsibble.dcmp_ts <- function(x, ...){
  new_tsibble(x)
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

#' @export
rbind.dcmp_ts <- function(...){
  dots <- dots_list(...)
  dcmp <- map(dots, function(x) x%@%"dcmp")
  resp <- map(dots, function(x) x%@%"resp")
  if(length(resp <- unique(resp)) > 1){
    abort("Decomposition response variables must be the same for all models.")
  }
  if(length(dcmp <- unique(dcmp)) > 1){
    warn("Batch decompositions contain different components. Using decomposition with most variables.")
    vars <- map(dcmp, all.vars)
    
    dcmp <- dcmp[[which.max(map_dbl(vars, length))]]
  }
  
  as_dable(invoke("rbind", map(dots, as_tsibble)),
           resp = !!resp[[1]], dcmp = !!dcmp)
}