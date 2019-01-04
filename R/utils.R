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

make_future_data <- function(object, h){
  lst_fits <- nest(group_by_key(fitted(select(object, !!((object%@%"models")[[1]])))))
  if(is.null(h)){
    h <- map_dbl(lst_fits$data, function(.x) get_frequencies("smallest", .x)*2)
  }
  if(is.character(h) || inherits(h, "Period")){
    require_package("lubridate")
    h <- map_dbl(lst_fits$data, function(.x) get_frequencies(lubridate::as.period(h), .x))
  }
  lst_fits[["new_data"]] <- map2(lst_fits$data, h, tsibble::new_data)
  lst_fits <- select(lst_fits, !!!key(object), new_data)
  unnest(lst_fits, new_data, key = key(object))
}

bind_new_data <- function(object, new_data){
  new_data <- new_data %>% 
    group_by(!!!syms(key_vars(object))) %>% 
    nest(.key = "new_data")
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