#' Create a new mable
#' 
#' @param data A tsibble used for modelling
#' @param model The fitted model
#' @param parsed_model The parsed model from `parse_model`
#'
#' @export
mable <- function(..., key = id(), models = id()){
  as_mable(tibble(...), key = key, models = models)
}

#' Coerce a dataset to a mable
#' 
#' @param x A dataset containing a list model column
#' @param key Structural variable(s) that identify each model
#' 
#' @rdname as_mable
#' @export
as_mable <- function(x, ...){
  UseMethod("as_mable")
}

#' @export
#' @rdname as_mable
as_mable.tbl_df <- function(x, key = id(), models = id(), ...){
  add_mdl_lst <- map(models, function(model) expr(add_class(!!model, "lst_mdl")))
  x <- mutate(x, !!!set_names(add_mdl_lst, map_chr(models, as_string)))
  structure(x, class = union("mdl_df", class(x)), key = key, models = models)
}

#' @importFrom tibble tbl_sum
#' @importFrom dplyr pull
#' @export
tbl_sum.mdl_df <- function(x){
  out <- c(`A mable` = sprintf("%s model%s", big_mark(NROW(x)), ifelse(NROW(x)==1, "", "s")))
  
  if(!is_empty(key(x))){
    out <- c(out, c("Key" = sprintf("%s [%i]",
                                    paste0(key_vars(x), collapse = ", "),
                                    n_keys(x))))
  }
  
  out
}

#' Provide a succinct summary of a model
#' 
#' Similarly to pillar's type_sum and obj_sum, model_sum is used to provide brief model summaries.
#' 
#' @param x The model to summarise
#' 
#' @export
model_sum <- function(x){
  UseMethod("model_sum")
}

#' @export
model_sum.default <- function(x){
  obj_sum(x)
}

#' @importFrom utils head capture.output
#' @export
summary.mdl_df <- function(object, ...){
  map(head(object$model), function(.x) capture.output(summary(.x))) %>%
    invoke(cat, ., sep="\n")
  invisible(object)
}

#' @export
key.mdl_df <- function(x){
  x%@%"key"
}

#' @export
key_vars.mdl_df <- function(x){
  map_chr(key(x), expr_text)
}

#' @export
n_keys.mdl_df <- function(x){
  key <- key_vars(x)
  if (is_empty(key)) {
    return(1L)
  }
  NROW(distinct(ungroup(as_tibble(x)), !!!syms(key)))
}