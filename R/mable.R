#' Create a new mable
#' 
#' @param data A tsibble used for modelling
#' @param model The fitted model
#' @param parsed_model The parsed model from `parse_model`
#'
#' @importFrom dplyr grouped_df
#'
#' @export
mable <- function(data, model, parsed_model){
  key_vals <- as.list(data)[key_vars(data)]
  data <- (data %>%
     grouped_df(key_vars(.)) %>%
     nest)$data
  new_mable(tibble(!!!key_vals, data=data,
                   model=list(enclass(model,
                                      "fable_model",
                                      fable = 
                                        list(
                                          model = parsed_model$model, 
                                          response = parsed_model$response,
                                          transformation = parsed_model$transformation)
                                        )
                              )
                   )
  )
}

#' Constructor
#' 
#' A constructor function for producing a mable (most useful for extension package authors)
#' 
#' @param x A mable-like object
#' 
#' @importFrom tibble new_tibble
#' 
#' @export
new_mable <- function(x){
  stopifnot(!is.null(x[["model"]]))
  if(!inherits(x[["model"]], "lst_mdl")){
    x[["model"]] <- add_class(x[["model"]], "lst_mdl")
  }
  new_tibble(x, subclass = c("mable", "lst_ts"))
}


#' Coerce a dataset to a mable
#' 
#' @param data A dataset containing a list model column
#' @param model A bare input containing the model column's name
#' 
#' @export
as_mable <- function(data, model){
  model <- enexpr(model)
  data %>%
    mutate(!!!list(model = expr(enclass(!!model, "lst_mdl")))) %>%
    enclass("mable")
}

#' @importFrom tibble tbl_sum
#' @importFrom tsibble key_sum
#' @importFrom dplyr pull
#' @export
tbl_sum.mable <- function(x){
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
  
  out <- c(`A mable` = sprintf("%s model%s [%s]", big_mark(NROW(x)), ifelse(NROW(x)==1, "", "s"), int_disp))
  
  if(!is_empty(key_vars(x))){
    out <- c(out, key_sum(x))
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

#' @importFrom utils head
#' @export
summary.mable <- function(object, ...){
  map(head(object$model), ~capture.output(summary(.x))) %>%
    invoke(cat, ., sep="\n")
  invisible(object)
}

#' @export
components.mable <- function(object, ...){
  object %>%
    transmute(
      !!!syms(key_vars(.)),
      components = map(object$model, components)
    ) %>%
    unnest(key = syms(key_vars(object)))
}

#' @export
key.mable <- key.dable

#' @export
key_vars.mable <- function(x){
  setdiff(colnames(x), c("data", "model"))
}

#' @export
n_keys.mable <- function (x){
  key <- key_vars(x)
  if (is_empty(key)) {
    return(1L)
  }
  NROW(distinct(ungroup(as_tibble(x)), !!!syms(key)))
}
