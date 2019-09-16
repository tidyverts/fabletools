#' Estimate models
#' 
#' Trains specified model definition(s) to a dataset. This function will 
#' estimate the a set of model definitions (passed via `...`) to each series
#' within `.data` (as identified by the key structure). The result will be a
#' mable (a model table), which neatly stores the estimated models in a tabular
#' structure. Rows of the data identify different series within the data, and
#' each model column contains all models from that model definition. Each cell
#' in the mable identifies a single model.
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
#' @section Parallel:
#' 
#' It is possible to estimate models in parallel using the
#' [future](https://cran.r-project.org/package=future) package. By specifying a
#' [`future::plan()`] before estimating the models, they will be computed 
#' according to that plan.
#' 
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' # Training an ETS(M,Ad,A) model to Australian beer production
#' aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A")))
#' 
#' # Training a seasonal naive and ETS(A,A,A) model to the monthly 
#' # "Food retailing" turnover for each Australian state/territory.
#' library(dplyr)
#' aus_retail %>% 
#'   filter(Industry == "Food retailing") %>% 
#'   model(
#'     snaive = SNAIVE(Turnover),
#'     ets = ETS(log(Turnover) ~ error("A") + trend("A") + season("A")),
#'   )
#' }
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
  .data <- nest_keys(.data, "lst_data")
  
  if(.safely){
    estimate <- function(dt, mdl){
      out <- safely(fabletools::estimate)(dt, mdl)
      if(is.null(out$result)){
        f <- quo(!!mdl$formula)
        f <- set_env(f, mdl$env)
        out$result <- fabletools::estimate(dt, null_model(!!f))
      }
      out
    }
  }
  
  if(is_attached("package:future")){
    require_package("furrr")
    eval_models <- function(models, lst_data){
      out <- furrr::future_map2(
        rep(lst_data, length(models)),
        rep(models, each = length(lst_data)),
        estimate, .progress = isTRUE(getOption("dplyr.show_progress")) && interactive() && is.null(getOption("knitr.in.progress"))
      )
      unname(split(out, rep(seq_len(num_mdl), each = num_key)))
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
  
  fits <- map(fits, add_class, "lst_mdl")
  
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
  
  if(is.formula(f)){
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
  if(is.formula(model$formula)){
    f_rhs(model$formula)
  }
  else{
    expr(NULL)
  }
}

#' @export
length.mdl_ts <-  function(x) 1