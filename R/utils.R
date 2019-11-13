# Small function to combine named lists
merge_named_list <- function(...){
  all_names <- dots_list(...) %>% map(names) %>% invoke(c, .) %>% unique
  all_names %>%
    map(function(name){
      dots_list(...) %>% map(function(vals) vals[[name]]) %>% invoke(c, .)
    }) %>%
    set_names(all_names)
}

add_class <- function(x, new_class){
  `class<-`(x, union(new_class, class(x)))
}

rm_class <- function(x, class){
  `class<-`(x, setdiff(class(x), class))
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

#' @importFrom tibble new_tibble
make_future_data <- function(.data, h = NULL){
  n <- get_frequencies(h, .data, .auto = "smallest")
  if(length(n) > 1){
    warn("More than one forecast horizon specified, using the smallest.")
    n <- min(n)
  }
  if(is.null(h)) n <- n*2
  
  # tsibble::new_data(.data, round(n))
  # Re-implemented here using a simpler/faster method
  n <- round(n)
  
  idx <- index_var(.data)
  itvl <- interval(.data)
  tunit <- time_unit(itvl)
  
  idx_max <- max(.data[[idx]])
  
  .data <- list2(!!idx := seq(idx_max, by = tunit, length.out = n+1)[-1])
  build_tsibble_meta(
    new_tibble(.data, nrow = n),
    key_data = new_tibble(list(.rows = list(seq_len(n))), nrow = 1),
    index = idx, index2 = idx, ordered = TRUE, interval = itvl
  )
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

# Unnest nested tibble/tsibble
# 
# Similar to tsibble::unnest_tsibble, but less general and faster validation
# 
# @param .data A dataset containing a listed column of tsibbles
# @param tbl_col/tsbl_col The column containing the tibble/tsibble to be unnested
# @param parent_key The keys from data to be joined with keys from the nested tsibble
# @param interval If NULL, the interval will be taken from the first tsibble, otherwise defaults to [[tsibble::build_tsibble()]] functionality.
unnest_tbl <- function(.data, tbl_col, .sep = NULL){
  row_indices <- rep.int(seq_len(NROW(.data)), map_int(.data[[tbl_col[[1]]]], NROW))
  
  nested_cols <- map(tbl_col, function(x){
    lst_col <- .data[[x]]
    if(is.data.frame(lst_col[[1]])){
      dplyr::bind_rows(!!!set_names(lst_col, rep(x, length(lst_col))))
    }
    else{
      list2(!!x := unlist(lst_col))
    }
  })
  
  if(!is.null(.sep)){
    nested_cols <- map2(
      nested_cols, tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }
  
  dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tbl_col), drop = FALSE], # Parent cols
    !!!nested_cols # Nested cols
  )
}

unnest_tsbl <- function(.data, tsbl_col, parent_key = NULL, interval = NULL){
  tsbl <- .data[[tsbl_col]][[1L]]
  if (!is_tsibble(tsbl)) {
    abort("Unnested column is not a tsibble object.")
  }
  idx <- index(tsbl)
  idx_chr <- as_string(idx)
  key <- c(parent_key, key_vars(tsbl))
  
  .data <- unnest_tbl(.data, tsbl_col)
  
  class(.data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(.data, key = !!key, index = !!idx,
                index2 = !!index2(tsbl), ordered = is_ordered(tsbl),
                interval = interval%||%interval(tsbl))
}

nest_grps <- function(.data, nm = "data"){
  out <- dplyr::group_data(.data)
  grps <- dplyr::group_vars(.data)
  row_indices <- out[[NCOL(out)]]
  out[[NCOL(out)]] <- NULL
  col_nest <- -match(grps, colnames(.data))
  if(is_empty(col_nest)){
    col_nest <- rlang::missing_arg()
  }
  out[[nm]] <- map(row_indices, function(x, i, j){
    out <- x[i,j]
  }, x = .data, j = col_nest)
  out
}

nest_keys <- function(.data, nm = "data"){
  out <- unclass(key_data(.data))
  key <- key_vars(.data)
  row_indices <- out[[length(out)]]
  out[[length(out)]] <- NULL
  col_nest <- -match(key, colnames(.data))
  if(is_empty(col_nest)){
    col_nest <- NULL
  }
  idx <- index_var(.data)
  idx2 <- index2_var(.data)
  ordered <- is_ordered(.data)
  regular <- is_regular(.data)
  out[[nm]] <- map(row_indices, function(x, i, j){
    out <- if(is.null(j)) x[i,] else x[i,j]
    build_tsibble_meta(
      out, 
      key_data = as_tibble(list(.rows = list(seq_along(i)))),
      index = idx, index2 = idx2, ordered = ordered, 
      interval = if(length(i) > 1 && regular) interval_pull(out[[idx]]) else interval(.data)
    )
  }, x = as_tibble(.data), j = col_nest)
  as_tibble(out)
}

bind_row_attrb <- function(x){
  attrb <- transpose(map(x, function(dt) map(dt, attributes)))
  simple_attrb <- map_lgl(attrb, function(x) length(unique(x)) == 1)
  
  x <- dplyr::bind_rows(!!!x)
  
  for (col in which(simple_attrb)){
    attributes(x[[col]]) <- attrb[[col]][[1]]
  }
  x
}

is.formula <- function(x) {
  inherits(x, "formula")
}