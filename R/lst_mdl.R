list_of_models <- function(x = list()){
  vctrs::new_vctr(x, class = "lst_mdl")
}

type_sum.lst_mdl <- function(x){
  "model"
}

#' @export
format.lst_mdl <- function(x, ...){
  map_chr(x, function(x) paste0("<", model_sum(x), ">"))
}

#' @export
vec_cast.character.lst_mdl <- function(x, to, ...) format(x)

#' @export
vec_ptype2.lst_mdl.lst_mdl <- function(x, y, ...){
  list_of_models()
}

#' @export
vec_cast.lst_mdl.lst_mdl <- function(x, to, ...){
  if(!identical(class(x), class(to))){
    abort("Cannot combine model lists with different reconciliation strategies.")
  }
  x
}