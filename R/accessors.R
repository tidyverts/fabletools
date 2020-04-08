response_vars <- function(x){
  stopifnot(is_fable(x))
  x%@%"response"
}

distribution_var <- function(x){
  stopifnot(is_fable(x))
  x%@%"dist"
}