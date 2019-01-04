#' Create a dable object
#'
#' @inheritParams tsibble::tsibble
#' @inheritParams fable
#' @param dcmp An expression of data columns defining how the variable was decomposed.
#'
#' @export
dable <- function(..., key = id(), index, resp, dcmp, structure = list(), regular = TRUE){
  tsbl <- tsibble(..., key = !!enquo(key), index = !!enexpr(index), regular = regular)
  as_dable(tsbl, !!enexpr(resp), !!enexpr(dcmp), structure = structure)
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
as_dable.tbl_ts <- function(x, resp, dcmp, structure = list(), ...){
  new_tsibble(x, resp = enexpr(resp), dcmp = enexpr(dcmp),
              structure = structure, class = "dcmp_ts")
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
  
  attrs <- combine_dcmp_attr(dots)
  
  as_dable(invoke("rbind", map(dots, as_tsibble)),
           resp = !!attrs[["response"]], dcmp = !!attrs[["decomposition"]],
           structure = attrs[["structure"]])
}

combine_dcmp_attr <- function(lst_dcmp){
  dcmp <- map(lst_dcmp, function(x) x%@%"dcmp")
  resp <- map(lst_dcmp, function(x) x%@%"resp")
  strc <- map(lst_dcmp, function(x) x%@%"structure")
  if(length(resp <- unique(resp)) > 1){
    abort("Decomposition response variables must be the same for all models.")
  }
  if(length(dcmp <- unique(dcmp)) > 1){
    warn("Batch decompositions contain different components. Using decomposition with most variables.")
    vars <- map(dcmp, all.vars)
    
    dcmp <- dcmp[which.max(map_dbl(vars, length))]
  }
  strc <- unlist(unique(strc), recursive = FALSE)
  strc <- strc[!duplicated(names(strc))]
  list(response = resp[[1]], decomposition = dcmp[[1]], structure = strc)
}