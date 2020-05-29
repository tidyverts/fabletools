#' Create a dable object
#'
#' A dable (decomposition table) data class (`dcmp_ts`) which is a tsibble-like
#' data structure for representing decompositions. This data class is useful for
#' representing decompositions, as its print method describes how its columns
#' can be combined to produce the original data, and has a more appropriate
#' `autoplot()` method for displaying decompositions. Beyond this, a dable
#' (`dcmp_ts`) behaves very similarly to a tsibble (`tbl_ts`).
#'
#' @inheritParams fable
#' @param ... Arguments passed to [tsibble::tsibble()].
#' @param method The name of the decomposition method.
#' @param seasons A named list describing the structure of seasonal components
#' (such as `period`, and `base`).
#' @param aliases A named list of calls describing common aliases computed from
#' components.
#'
#' @export
dable <- function(..., response, method = NULL, seasons = list(), aliases = list()){
  build_dable(tsibble(...), method = method, response = !!enquo(response), 
              seasons = seasons, aliases = aliases)
}

#' Is the object a dable
#' 
#' @param x An object.
#' 
#' @export
is_dable <- function(x){
  inherits(x, "dcmp_ts")
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
#' @export
as_dable.tbl_df <- function(x, response, method = NULL, seasons = list(), aliases = list(), ...){
  build_dable(x, method = method, response = !!enquo(response), 
              seasons = seasons, aliases = aliases)
}

#' @rdname as-dable
#' 
#' @inheritParams dable
#' 
#' @export
as_dable.tbl_ts <- function(x, response, method = NULL, seasons = list(), aliases = list(), ...){
  build_dable(x, method = method, response = !!enquo(response), 
              seasons = seasons, aliases = aliases)
}

build_dable <- function (x, response, method = NULL, seasons = list(), aliases = list()) {
  response <- names(x)[tidyselect::eval_select(enquo(response), x)]
  new_tsibble(x, method = method, response = response, 
              seasons = seasons, aliases = aliases, class = "dcmp_ts")
}

#' @export
as_tsibble.dcmp_ts <- function(x, ...){
  new_tsibble(x)
}

#' @export
`[.dcmp_ts` <- function (x, i, j, drop = FALSE){
  out <- NextMethod()
  # Drop dable if tsibble is dropped
  
  cn <- colnames(out)
  not_dable <- !(response_vars(x) %in% cn) || !is_tsibble(out)
  
  if(not_dable)
    return(out)
  else
    as_dable(out, response = response_vars(x), method = x%@%"method",
             seasons = x%@%"seasons", aliases = x%@%"aliases")
}

tbl_sum.dcmp_ts <- function(x){
  response <- response_vars(x)
  method <- expr_text((x%@%"aliases")[[response]])
  out <- NextMethod()
  names(out)[1] <- "A dable"
  append(out, set_names(paste(response, method, sep = " = "),
                        paste(x%@%"method", "Decomposition")))
}

#' @export
rbind.dcmp_ts <- function(...){
  .Deprecated("bind_rows()")
  dots <- dots_list(...)
  
  attrs <- combine_dcmp_attr(dots)
  
  as_dable(invoke("rbind", map(dots, as_tsibble)), 
           method = attrs[["method"]], response = !!attrs[["response"]], 
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

combine_dcmp_attr <- function(lst_dcmp){
  resp <- map(lst_dcmp, response_vars)
  method <- map(lst_dcmp, function(x) x%@%"method")
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
  
  list(response = resp[[1]], method = paste0(unique(method), collapse = " & "),
       seasons = strc, aliases = aliases)
}