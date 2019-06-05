expr_sym <- function(expr){
  sym(expr_name(expr))
}

quo_sym <- function(quo){
  sym(quo_name(quo))
}

# Small function to combine named lists
merge_named_list <- function(...){
  all_names <- dots_list(...) %>% map(names) %>% invoke(c, .) %>% unique
  all_names %>%
    map(function(name){
      dots_list(...) %>% map(function(vals) vals[[name]]) %>% invoke(c, .)
    }) %>%
    set_names(all_names)
}

merge_pos_list <- function(...){
  all_pos <- dots_list(...) %>% map(seq_along) %>% invoke(c, .) %>% unique
  all_pos %>%
    map(function(pos){
      dots_list(...) %>% map(function(vals) vals[[pos]]) %>% invoke(c, .)
    }) %>%
    set_names(names(dots_list(...)[[1]]))
}

enclass <- function(x, subclass = NULL, ...){
  dots_list(...) %>%
    imap(function(value, name) set_names(list(value), name)) %>%
    reduce(.init = x, # Add attributes (from ...)
           function(x, attr) {
             if (!is.null(attr[[1]])) {
               attr(x, names(attr)) <- attr[[1]]
             }
             x
           }) %>%
    add_class(subclass)
}

add_class <- function(x, new_class){
  `class<-`(x, union(new_class, class(x)))
}

rm_class <- function(x, class){
  `class<-`(x, class(x)[!(class(x) %in% class)])
}

exclude <- function(match, vars = tidyselect::peek_vars()){
  vars[-match(match, vars)]
}

custom_error <- function(.f, error){
  force(error)
  function(...){
    res <- capture_error(.f(...))
    if(!is.null(res$error)){
      abort(error)
    }
    res$result
  }
}

make_future_data <- function(.data, h = NULL){
  n <- get_frequencies(h, .data, .auto = "smallest")
  if(length(n) > 1){
    warn("More than one forecast horizon specified, using the smallest.")
    n <- min(n)
  }
  if(is.null(h)) n <- n*2
  tsibble::new_data(.data, round(n))
}

bind_new_data <- function(object, new_data){
  if(!is.data.frame(new_data)){
    abort(sprintf("`new_data` requires a data frame. Perhaps you intended to specify the forecast horizon? If so, use `h = %s`.", deparse(new_data)))
  }
  if(!identical(key_vars(object), key_vars(new_data))){
    abort("Provided data contains a different key structure to the models.")
  }
  
  new_data <- nest_keys(new_data, "new_data")
  
  if(length(key_vars(object)) > 0){
    object <- left_join(object, new_data, by = key_vars(object))
  }
  else{
    object[["new_data"]] <- new_data[["new_data"]]
  }
  object
}

# From R6
assign_func_envs <- function(objs, target_env) {
  if (!is.null(target_env)){
    objs <- lapply(objs, function(x) {
      if (is.function(x)) environment(x) <- target_env
      x
    })
  }
  objs
}

# tibble:::big_mark
big_mark <- function (x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) 
    "."
  else ","
  ret <- formatC(x, big.mark = mark, ...)
  ret[is.na(x)] <- "??"
  ret
}

require_package <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}

calc <- function(f, ...){
  f(...)
}

is_dist_normal <- function(dist){
  identical(dist[[1]]$.env$f, env_dist_normal$f) && !dist[[1]]$.env$trans
}

#' Unnest into a tsibble
#' 
#' Similar to tsibble::unnest_tsibble, but less general and faster validation
#' 
#' @param .data A dataset containing a listed column of tsibbles
#' @param tsbl_col The column containing the tsibble to be unnested
#' @param parent_key The keys from data to be joined with keys from the nested tsibble
#' @param interval If NULL, the interval will be taken from the first tsibble, otherwise defaults to [[tsibble::build_tsibble()]] functionality.
unnest_tsbl <- function(.data, tsbl_col, parent_key = NULL, interval = NULL){
  tsbl <- .data[[tsbl_col]][[1L]]
  if (!is_tsibble(tsbl)) {
    abort("Unnested column is not a tsibble object.")
  }
  idx <- index(tsbl)
  
  row_indices <- rep.int(seq_len(NROW(.data)), map_int(.data[[tsbl_col]], NROW))
  .data <- dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tsbl_col)], # Parent cols
    dplyr::bind_rows(!!!.data[[tsbl_col]]) # Nested cols
  )
  
  key <- c(parent_key, key_vars(tsbl))
  idx_chr <- as_string(idx)
  class(.data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(.data, key = !!key, index = !!idx, 
                index2 = !!index2(tsbl), ordered = is_ordered(tsbl), 
                interval = interval%||%interval(tsbl))
}

nest_keys <- function(.data, nm = "data"){
  out <- key_data(.data)
  key <- key_vars(.data)
  row_indices <- out[[NCOL(out)]]
  out[[NCOL(out)]] <- NULL
  col_nest <- -match(key, colnames(.data))
  if(is_empty(col_nest)){
    col_nest <- rlang::missing_arg()
  }
  out[[nm]] <- map(row_indices, function(x, i, j){
    build_tsibble(x[i,j], index = !!index(x), interval = is_regular(x), validate = FALSE)
  }, x = .data, j = col_nest)
  out
}