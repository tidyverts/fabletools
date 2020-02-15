list_of_models <- function(x){
  vctrs::new_vctr(x, class = "lst_mdl")
}

type_sum.lst_mdl <- function(x){
  "model"
}

#' @export
format.lst_mdl <- function(x, ...){
  x %>% map_chr(model_sum) %>% map(function(x) paste0("<", x, ">"))
}
