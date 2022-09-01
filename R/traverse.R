#' Recursively traverse an object
#' 
#' @param x The object to traverse
#' @param .f A function for combining the recursed components
#' @param .g A function applied to the object before recursion
#' @param .h A function applied to the base case
#' @param base The base case for the recursion
#' 
#' @keywords internal
traverse <- function(x, .f = list, .g = identity, .h = identity, base = function(.x) is_syntactic_literal(.x) || is_symbol(.x)){
  # base case
  if(base(x))
    return(.h(x))
  # recursive case
  .f(lapply(.g(x), traverse, .f=.f, .g=.g, .h=.h, base=base), .h(x))
}

traverse_list <- function(x,
                          .f = function(.x, .y) as.list(.x),
                          .g = identity, 
                          .h = identity, 
                          base = function(.x) !is.list(.x)){
  traverse(x, .f=.f, .g=.g, .h=.h, base=base)
}

traverse_call <- function(x,
                          .f = function(.x, .y) map(.x, quo_get_expr) %>% as.call %>% new_quosure(env = get_env(.x[[1]])), # (quo_listcall_to_call)
                          .g = function(.x) .x %>% get_expr %>% as.list %>% map(new_quosure, env = get_env(.x)), # (quo_call_to_listcall)
                          .h = identity,
                          base = function(.x) !quo_is_call(.x)){
  x <- enquo(x)
  traverse(x, .f=.f, .g=.g, .h=.h, base=base)
}
