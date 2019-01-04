#' Create a new mable
#' 
#' @inheritParams tibble::tibble
#' 
#' @param key Structural variable(s) that identify each model.
#' @param models Identifiers for the columns containing model(s).
#'
#' @export
mable <- function(..., key = id(), models = id()){
  as_mable(tibble(...), key = key, models = models)
}

#' Coerce a dataset to a mable
#' 
#' @param x A dataset containing a list model column.
#' @param ... Additional arguments passed to other methods.
#' 
#' @rdname as_mable
#' @export
as_mable <- function(x, ...){
  UseMethod("as_mable")
}

#' @rdname as_mable
#' 
#' @param key Structural variable(s) that identify each model.
#' @param models Identifiers for the columns containing model(s).
#' 
#' @export
as_mable.tbl_df <- function(x, key = id(), models = id(), ...){
  add_mdl_lst <- map(models, function(model) expr(add_class(!!model, "lst_mdl")))
  x <- mutate(x, !!!set_names(add_mdl_lst, map_chr(models, as_string)))
  tibble::new_tibble(x, key = key, models = models, subclass = "mdl_df")
}

#' @export
as_tibble.mdl_df <- function(x, ...){
  attr(x, "key") <- attr(x, "models") <- NULL
  class(x) <- c("tbl_df", "tbl", "data.frame")
  as_tibble(x, ...)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.mdl_df <- function(x){
  out <- c(`A mable` = paste(map_chr(dim(x), big_mark), collapse = " x "))
  
  if(!is_empty(key(x))){
    out <- c(out, c("Key" = sprintf("%s [%i]",
                                    paste0(key_vars(x), collapse = ", "),
                                    n_keys(x))))
  }
  
  out
}

#' @export
gather.mdl_df <- function(data, key = "key", value = "value", ..., na.rm = FALSE,
                          convert = FALSE, factor_key = FALSE){
  key <- sym(enexpr(key))
  value <- enexpr(value)
  tbl <- gather(as_tibble(data), key = !!key, value = !!value, 
                ..., na.rm = na.rm, convert = convert, factor_key = factor_key)
  
  mdls <- syms(names(which(map_lgl(tbl, inherits, "lst_mdl"))))
  as_mable(tbl, key = c(key(data), key), models = mdls)
}

#' @export
select.mdl_df <- function (.data, ...){
  key <- key(.data)
  .data <- select(as_tibble(.data), !!!key(.data), ...)
  
  mdls <- syms(names(which(map_lgl(.data, inherits, "lst_mdl"))))
  if(is_empty(mdls)){
    abort("A mable must contain at least one model. To remove all models, first convert to a tibble with `as_tibble()`.")
  }
  as_mable(.data, key = key, models = mdls)
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