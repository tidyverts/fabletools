### lst_mdl ###

type_sum.lst_mdl <- function(x){
  "model"
}

#' @export
format.lst_mdl <- function(x, ...){
  x %>% map_chr(model_sum) %>% map(function(x) paste0("<", x, ">"))
}

#' @export
c.lst_mdl <- function(x, ...){
  add_class(NextMethod(), "lst_mdl")
}

#' @export
`[.lst_mdl` <- c.lst_mdl

#' @export
print.lst_mdl <- function(x, ...){
  print(map_chr(x, model_sum), quote = FALSE)
}
