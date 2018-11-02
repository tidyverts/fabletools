#' Create a new dable
#' 
#' @param data A tsibble used for decomposition
#' @param decomposition The decomposition table
#' @param parsed_model The parsed decomposition from `parse_model`
#'
#' @importFrom dplyr grouped_df
#'
#' @export
dable <- function(data, decomposition, parsed_model){
  key_vals <- as.list(data)[key_vars(data)]
  data <- (data %>%
             grouped_df(key_vars(.)) %>%
             nest)$data
  new_dable(tibble(!!!key_vals, data=data,
                   decomposition=list(
                     enclass(decomposition,
                             model = parsed_model$model, 
                             response = parsed_model$response,
                             transformation = parsed_model$transformation)
                   )
  )
  )
}

#' Constructor
#' 
#' A constructor function for producing a dable (most useful for extension package authors)
#' 
#' @param x A dable-like object
#' 
#' @export
new_dable <- function(x){
  stopifnot(!is.null(x[["decomposition"]]))
  if(!inherits(x[["decomposition"]], "lst_dcmp")){
    x[["decomposition"]] <- add_class(x[["decomposition"]], "lst_dcmp")
  }
  new_tibble(x, subclass = c("dable", "lst_ts"))
}

#' Coerce a dataset to a dable
#' 
#' @param data A dataset containing a list decomposition column
#' @param decomposition A bare input containing the decomposition column's name
#' 
#' @export
as_dable <- function(data, decomposition){
  decomposition <- enexpr(decomposition)
  data %>%
    mutate(!!!list(decomposition = expr(enclass(!!decomposition, "lst_mdl")))) %>%
    enclass("mable")
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.dable <- function(x){
  intervals <- x %>%
    pull(!!sym("data")) %>%
    map(interval) %>%
    unique
  if(length(intervals)==1){
    int_disp <- format(intervals[[1]])
  }
  else{
    int_disp <- "MIXED"
  }
  
  out <- c(`A dable` = sprintf("[%s]", int_disp))
  
  if(!is_empty(key(x))){
    out <- c(out, c("Key" = sprintf("[%s]", paste0(key_vars(x), collapse = ","))))
  }
  
  out
}

#' @export
components.dable <- function(object, ...){
  keys <- syms(key_vars(object))
  object %>%
    transmute(
      !!!keys,
      components = map(object$decomposition, components)
    ) %>%
    unnest(key = keys)
}

#' @importFrom tsibble key
#' @export
key.dable <- function(x){
  enclass(syms(key_vars(x)), "key")
}

#' @export
key_vars.dable <- function(x){
  setdiff(colnames(x), c("data", "decomposition"))
}

#' @export
n_keys.dable <- function (x){
  key <- key_vars(x)
  if (is_empty(key)) {
    return(1L)
  }
  NROW(distinct(ungroup(as_tibble(x)), !!!syms(key)))
}