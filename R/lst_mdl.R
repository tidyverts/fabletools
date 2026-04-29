list_of_models <- function(x = list()){
  vctrs::new_vctr(x, class = c("mdl_lst", "lst_mdl"))
}

type_sum.mdl_lst <- function(x){
  "model"
}

#' @export
format.mdl_lst <- function(x, ...){
  map_chr(x, function(x) paste0("<", model_sum(x), ">"))
}
#' @export
format.lst_mdl <- deprecate_lst_mdl(format.mdl_lst)

#' @export
vec_cast.character.mdl_lst <- function(x, to, ...) format(x)
#' @export
vec_cast.character.lst_mdl <- deprecate_lst_mdl(vec_cast.character.mdl_lst)

#' @export
vec_ptype2.mdl_lst.mdl_lst <- function(x, y, ...){
  list_of_models()
}

#' @export
vec_cast.mdl_lst.mdl_lst <- function(x, to, ...){
  if(!identical(class(x), class(to))){
    abort("Cannot combine model lists with different reconciliation strategies.")
  }
  x
}