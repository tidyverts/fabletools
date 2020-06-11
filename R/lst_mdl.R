list_of_models <- function(x){
  vctrs::new_vctr(x, class = "lst_mdl")
}

type_sum.lst_mdl <- function(x){
  "model"
}

#' @export
format.lst_mdl <- function(x, ...){
  map_chr(x, function(x) paste0("<", model_sum(x), ">"))
}
