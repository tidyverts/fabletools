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
    map(models, function(model){
      validate_formula(model, lst_data[[1]])
      map(lst_data, function(data){
        model$data <- data
        parsed <- parse_model(model)
        data <- transmute(data, !!model_lhs(parsed$model))
        fit <- eval_tidy(
          expr(model$train(.data = data, formula = model$formula,
                               specials = parsed$specials, !!!model$extra))
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
  type_sum(x)
}

#' @export
model_sum.model <- function(x){
  model_sum(x$fit)
}

#' Extract the left hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_lhs <- function(model){
  if(is_formula(model$formula)){
    f_lhs(model$formula)
  }
  else{
    model$formula
  }
}

#' Extract the right hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_rhs <- function(model){
  if(is_formula(model$formula)){
    f_rhs(model$formula)
  }
  else{
    expr(NULL)
  }
}