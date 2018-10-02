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

bind_new_data <- function(object, new_data){
  if(is.null(new_data)){
    new_data <- map_dbl(object$data, function(.x) get_frequencies("smallest", .x)*2)
  }
  if(is.numeric(new_data)){
    object[["new_data"]] <- map2(object$data, new_data,
                                function(data, h){
                                  idx <- expr_text(index(data))
                                  future <- fc_idx(data[[idx]], h)
                                  build_tsibble(list2(!!idx := future), key = id(), index = idx)
                                })
  }
  else{
    new_data <- new_data %>% 
      group_by(!!!syms(key_vars(object))) %>% 
      nest(.key = "new_data")
    if(length(key_vars(object)) > 0){
      object <- left_join(object, new_data, by = key_vars(object))
    }
    else{
      object[["new_data"]] <- new_data[["new_data"]]
    }
  }
  object
}

key_add <- function(object, ...){
  .key <- structure(append(key(object), enexprs(...)), class = class(key(object)))
  as_tsibble(object, validate = TRUE, key = .key, index = !!index(object))
}