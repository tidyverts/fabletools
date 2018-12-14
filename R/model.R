#' Estimate models
#' 
#' @param .data A data structure suitable for the models (such as a `tsibble`)
#' @param ... Definitions for the models to be used
#' 
#' @export
model <- function(.data, ...){
  UseMethod("model")
}

#' @export
model.tbl_ts <- function(.data, ...){
  nm <- map(enexprs(...), expr_text)
  models <- dots_list(...)
  
  keys <- key(.data)
  .data <- nest(group_by(.data, !!!keys), .key = "lst_data")
  
  eval_models <- function(models, lst_data){
    map(models, function(model_def){
      model_def$formula <- validate_model(model_def$formula, lst_data[[1]])
      map(lst_data, function(data){
        parsed <- parse_model(data, model_def$formula, model_def$specials)
        data <- transmute(data, !!model_lhs(parsed$model))
        fit <- eval_tidy(
          expr(model_def$train(.data = data, formula = model_def$formula,
                               specials = parsed$specials, !!!model_def$dots))
        )
        new_model(fit, parsed$response, parsed$transformation)
      })
    })
  }
  
  fits <- eval_models(models, .data[["lst_data"]])
  names(fits) <- ifelse(nchar(names(fits)), names(fits), nm)
  
  .data %>% 
    transmute(
      !!!keys,
      !!!fits
    ) %>% 
    as_mable(keys, syms(names(fits)))
}

new_model <- function(fit, response, transformation){
  structure(list(fit = fit, 
                 response = response, transformation = transformation),
            class = "model")
}

#' @export
model_sum.model <- function(x){
  model_sum(x$fit)
}

#' Define a model
#' 
#' @param train A function used to train the model to data
#' @param specials A list of functions used to be evaluated from the formula.
#' If specials is NULL, no specials are computed
#' 
#' @export
define_model <- function(train, specials){
  force(train)
  force(specials)
  function(formula, ...){
    structure(
      list(
        formula = enquo(formula),
        train = train, 
        specials = specials, 
        dots = list(...)),
      class = "model_definition"
    )
  }
}

#' Extract the left hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_lhs <- function(model){
  if(is_formula(model)){
    transform_spec <- f_lhs(model)
  }
  else{
    transform_spec <- model
  }
}

#' Extract the right hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_rhs <- function(model){
  if(is_formula(model)){
    f_rhs(model)
  }
  else{
    expr(NULL)
  }
}