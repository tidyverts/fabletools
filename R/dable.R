#' Create a dable object
#'
#' @inheritParams tsibble::tsibble
#' @inheritParams fable
#' @param seasons A named list describing the structure of seasonal components
#' (such as `period`, and `base`).
#' @param aliases A named list of calls describing common aliases computed from
#' components.
#'
#' @export
dable <- function(..., key = id(), index, resp, seasons = list(), aliases = list(),
                  regular = TRUE){
  tsbl <- tsibble(..., key = !!enquo(key), index = !!enexpr(index), regular = regular)
  as_dable(tsbl, !!enexpr(resp), seasons = seasons, aliases = aliases)
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
as_dable.tbl_ts <- function(x, resp, seasons = list(), aliases = list(), ...){
  new_tsibble(x, resp = enexpr(resp), seasons = seasons, aliases = aliases,
              class = "dcmp_ts")
}

#' @export
as_tsibble.dcmp_ts <- function(x, ...){
  new_tsibble(x)
}

#' @importFrom tsibble tbl_sum
#' @export
tbl_sum.dcmp_ts <- function(x){
  response <- expr_text(x%@%"resp")
  method <- expr_text((x%@%"aliases")[[response]])
  
  out <- NextMethod()
  names(out)[1] <- "A dable"
  append(out,
         c("Decomposition" = paste(response, method, sep = " = ")))
}

#' @export
rbind.dcmp_ts <- function(...){
  dots <- dots_list(...)
  
  attrs <- combine_dcmp_attr(dots)
  
  as_dable(invoke("rbind", map(dots, as_tsibble)), resp = !!attrs[["response"]], 
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

combine_dcmp_attr <- function(lst_dcmp){
  resp <- map(lst_dcmp, function(x) x%@%"resp")
  strc <- map(lst_dcmp, function(x) x%@%"seasons")
  aliases <- map(lst_dcmp, function(x) x%@%"aliases")
  if(length(resp <- unique(resp)) > 1){
    abort("Decomposition response variables must be the same for all models.")
  }
  
  strc <- unlist(unique(strc), recursive = FALSE)
  strc <- strc[!duplicated(names(strc))]
  
  aliases <- unlist(unique(aliases), recursive = FALSE)
  aliases <- split(aliases, names(aliases)) %>% 
    map(function(x){
      vars <- map(x, all.vars)
      x[[which.max(map_dbl(vars, length))]]
    })
  
  list(response = resp[[1]], seasons = strc, aliases = aliases)
}