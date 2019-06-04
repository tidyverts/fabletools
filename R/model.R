#' Estimate models
#' 
#' Trains specified model definitions to a dataset.
#' 
#' @param .data A data structure suitable for the models (such as a `tsibble`)
#' @param ... Definitions for the models to be used
#'
#' @rdname model
#' @export
model <- function(.data, ...){
  UseMethod("model")
}

#' @rdname model
#' 
#' @param .safely If a model encounters an error, rather than aborting the process a [NULL model][null_model()] will be returned instead. This allows for an error to occur when computing many models, without losing the results of the successful models.
#' 
#' @export
model.tbl_ts <- function(.data, ..., .safely = TRUE){
  nm <- map(enexprs(...), expr_text)
  models <- dots_list(...)
  
  if(length(models) == 0){
    abort("At least one model must be specified.")
  }
  if(!all(is_mdl <- map_lgl(models, inherits, "mdl_defn"))){
    abort(sprintf("Model definition(s) incorrectly created: %s
Check that specified model(s) are model definitions.", nm[which(!is_mdl)[1]]))
  }
  
  num_key <- n_keys(.data)
  num_mdl <- length(models)
  num_est <- num_mdl * num_key
  pb <- dplyr::progress_estimated(num_est, min_time = 5)
  
  keys <- key(.data)
  .data <- nest(group_by(.data, !!!keys), .key = "lst_data")
  
  if(.safely){
    estimate <- function(dt, mdl){
      out <- safely(fablelite::estimate)(dt, mdl)
      if(is.null(out$result)){
        f <- quo(!!mdl$formula)
        f <- set_env(f, mdl$env)
        out$result <- fablelite::estimate(dt, null_model(!!f))
      }
      out
    }
  }
  
  if(is_attached("package:future")){
    require_package("future")
    eval_models <- function(models, lst_data){
      out <- vector("list", num_est)
      for(i in seq_len(num_est)){
        tsbl <- mdl <- NULL
        out[[i]] <- future::future(
          {
            estimate(tsbl, mdl)
          },
          globals = list(
            tsbl = lst_data[[1 + (i-1)%%num_key]],
            mdl = models[[1 + (i-1)%/%num_key]],
            estimate = estimate
          )
        )
      }
      unname(split(future::values(out), rep(seq_len(num_mdl), each = num_key)))
    }
  }
  else{
    eval_models <- function(models, lst_data){
      map(models, function(model){
        map(lst_data, function(dt, mdl){
          out <- estimate(dt, mdl)
          pb$tick()$print()
          out
        }, model)
      })
    }
  }
  
  fits <- eval_models(models, .data[["lst_data"]])
  names(fits) <- ifelse(nchar(names(models)), names(models), nm)
  
  # Report errors if estimated safely
  if(.safely){
    fits <- imap(fits, function(x, nm){
      err <- map_lgl(x, function(x) !is.null(x[["error"]]))
      if((tot_err <- sum(err)) > 0){
        err_msg <- table(map_chr(x[err], function(x) x[["error"]][["message"]]))
        warn(
          sprintf("%i error%s encountered for %s\n%s\n",
                  tot_err,
                  if(tot_err > 1) sprintf("s (%i unique)", length(err_msg)) else "", 
                  nm,
                  paste0("[", err_msg, "] ", names(err_msg), collapse = "\n")
          )
        )
      }
      map(x, function(x) x[["result"]])
    })
  }
  
  .data %>% 
    transmute(
      !!!keys,
      !!!fits
    ) %>% 
    as_mable(keys, names(fits))
}

new_model <- function(fit, model, data, response, transformation){
  if(is_model(fit)) return(fit)
  structure(list(fit = fit, model = model, data = data,
                 response = response, transformation = transformation),
            class = "mdl_ts")
}

#' Is the object a model
#' 
#' @param x An object.
#' 
#' @export
is_model <- function(x){
  inherits(x, "mdl_ts")
}

type_sum.mdl_ts <-  function(x){
  model_sum(x[["fit"]])
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
  tibble::type_sum(x)
}

#' @export
model_sum.mdl_ts <-  function(x){
  model_sum(x$fit)
}

#' @export
print.mdl_ts <-  function(x, ...){
  report(x)
}

#' Extract the left hand side of a model
#' 
#' @param model A formula
#' 
#' @export
model_lhs <- function(model){
  f <- model$formula
  if(is_quosure(f)){
    f <- get_expr(f)
  }
  
  if(is_formula(f)){
    f_lhs(f)
  }
  else{
    f
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

#' @export
formula.mdl_ts <-  function(x, ...){
  x$formula
}

#' @export
length.mdl_ts <-  function(x) 1