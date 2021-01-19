#' Create a new mable
#' 
#' A mable (model table) data class (`mdl_df`) is a tibble-like data structure 
#' for applying multiple models to a dataset. Each row of the mable refers to a
#' different time series from the data (identified by the key columns). A mable
#' must contain at least one column of time series models (`mdl_ts`), where the
#' list column itself (`lst_mdl`) describes how these models are related.
#' 
#' @inheritParams tibble::tibble
#' 
#' @param key Structural variable(s) that identify each model.
#' @param model Identifiers for the columns containing model(s).
#'
#' @export
mable <- function(..., key = NULL, model = NULL){
  as_mable(tibble(...), key = !!enquo(key), model = !!enquo(model))
}

#' Is the object a mable
#' 
#' @param x An object.
#' 
#' @export
is_mable <- function(x){
  inherits(x, "mdl_df")
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
#' @inheritParams mable
#' 
#' @export
as_mable.data.frame <- function(x, key = NULL, model = NULL, ...){
  build_mable(x, key = !!enquo(key), model = !!enquo(model))
}

build_mable <- function (x, key = NULL, key_data = NULL, model = NULL) {
  model <- names(tidyselect::eval_select(enquo(model), data = x))
  
  if(length(resp_var <- unique(map(x[model], function(mdl) response_vars(mdl[[1]])))) > 1){
    abort("A mable can only contain models with the same response variable(s).")
  }
  if(length(resp_var) == 0) {
    abort("A mable must contain at least one model.")
  }
  
  if (!is_null(key_data)){
    assert_key_data(key_data)
    key <- utils::head(names(key_data), -1L)
  }
  else {
    key <- names(tidyselect::eval_select(enquo(key), data = x))
    key_data <- group_data(group_by(x, !!!syms(key)))
  }
  
  if(any(map_int(key_data[[length(key_data)]], length) > 1)){
    abort("The result is not a valid mable. The key variables must uniquely identify each row.")
  }
  
  build_mable_meta(x, key_data, model, response = resp_var[[1]])
}

build_mable_meta <- function(x, key_data, model, response){
  tibble::new_tibble(x, key = key_data, model = model, response = response,
                     nrow = NROW(x), class = "mdl_df", subclass = "mdl_df") 
}

#' @export
as_tibble.mdl_df <- function(x, ...){
  attr(x, "key") <- attr(x, "model") <- NULL
  class(x) <- c("tbl_df", "tbl", "data.frame")
  as_tibble(x, ...)
}

tbl_sum.mdl_df <- function(x){
  out <- c(`A mable` = paste(map_chr(dim(x), big_mark), collapse = " x "))
  
  if(!is_empty(key(x))){
    out <- c(out, c("Key" = sprintf("%s [%s]",
                                    paste0(key_vars(x), collapse = ", "),
                                    map_chr(n_keys(x), big_mark))))
  }
  
  out
}

restore_mable <- function(data, template){
  data <- as_tibble(data)
  data_cols <- names(data)
  
  # key_vars <- setdiff(key_vars(template), data_cols)
  # key_data <- select(key_data(template), key_vars)
  # if (vec_size(key_data) == 1) {
  #   template <- remove_key(template, setdiff(key_vars(template), key_vars))
  # }
  
  model_vars <- intersect(mable_vars(template), data_cols)
  # Variables to keep
  mbl_vars <- setdiff(key_vars(template), data_cols)
  res <- bind_cols(template[mbl_vars], data)
  
  build_mable(res, key = !!key_vars(template), model = !!model_vars)
}

#' @export
gather.mdl_df <- function(data, key = "key", value = "value", ..., na.rm = FALSE,
                          convert = FALSE, factor_key = FALSE){
  value <- enexpr(value)
  tbl <- gather(as_tibble(data), key = !!key, value = !!value, 
                ..., na.rm = na.rm, convert = convert, factor_key = factor_key)
  mdls <- names(which(map_lgl(tbl, inherits, "lst_mdl")))
  kv <- c(key_vars(data), key)
  build_mable(tbl, key = !!kv, model = !!mdls)
}

# Adapted from tsibble:::pivot_longer.tbl_ts
#' @importFrom tidyr pivot_longer
#' @export
pivot_longer.mdl_df <- function (data, ..., names_to = "name") {
  if (!has_length(names_to)) {
    abort("`pivot_longer(<mable>)` can't accept zero-length `names_to`.")
  }
  if (".value" %in% names_to) {
    abort("`pivot_longer(<mable>)` can't accept the special \".value\" in `names_to`.")
  }
  new_key <- c(key_vars(data), names_to)
  tbl <- tidyr::pivot_longer(as_tibble(data), ..., names_to = names_to)
  build_mable(tbl, key = !!new_key,
              model = !!which(vapply(tbl, inherits, logical(1L), "lst_mdl")))
}

#' @export
select.mdl_df <- function (.data, ...){
  res <- NextMethod()
  
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  
  kv <- key_vars(.data)
  rm_kv <- intersect(kv, names(.data)[-loc])
  key_data <- vec_unique(select(key_data(.data), rm_kv))
  
  # Drop/keep redundant/necessary key variables
  if(vec_size(key_data) == 1) {
    .data%@%"key" <- key_data(.data)[c(setdiff(kv, rm_kv), ".rows")]
  } else {
    res <- bind_cols(.data[rm_kv], res)
  }
  names(.data)[loc] <- names(loc)
  
  restore_mable(res, .data)
}
#' @export
transmute.mdl_df <- function (.data, ...){
  nm <- names(enquos(..., .named = TRUE))
  res <- mutate(.data, ...)
  select(res, !!nm)
}

#' @export
`$<-.mdl_df` <- function (x, name, value) {
  tbl <- NextMethod()
  mdls <- names(which(map_lgl(tbl, inherits, "lst_mdl")))
  as_mable(tbl, key = key_vars(x), model = mdls)
}

#' @export
`names<-.mdl_df` <- function(x, value) {
  nm <- colnames(x)
  key_pos <- match(key_vars(x), nm)
  kd <- key_data(x)
  colnames(kd) <- c(value[key_pos], ".rows")
  mdl_pos <- match(mable_vars(x), nm)
  res <- NextMethod()
  build_mable_meta(res, key_data = kd, model = value[mdl_pos], 
                   response = response_vars(x))
}

#' @export
`[.mdl_df` <- function (x, i, j, drop = FALSE){
  out <- as_tibble(NextMethod())
  cn <- colnames(out)
  kv <- intersect(key_vars(x), cn)
  mv <- intersect(mable_vars(x), cn)
  
  # If keys have been dropped, return a tibble
  if(n_keys(x) != length(kv)){
    return(out)
  }
  
  build_mable(out, key = !!kv, model = !!mv)
}

#' @export
group_data.mdl_df <- function(.data){
  .data <- as_tibble(.data)
  NextMethod()
}

#' @export
key_data.mdl_df <- function(x){
  x%@%"key"
}

#' @export
key_vars.mdl_df <- function(x){
  keys <- key_data(x)
  names(keys)[-NCOL(keys)]
}

#' @export
key.mdl_df <- function(x){
  syms(key_vars(x))
}
