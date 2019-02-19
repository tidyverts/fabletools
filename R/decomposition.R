globalVariables("self")

train_decomposition <- function(.data, formula, specials, ...){
  # Extract raw original data
  est <- .data
  .data <- self$data
  
  dcmp <- do.call(self$dcmp_fn, list2(.data, formula, !!!self$dcmp_args))
  
  dcmp_method <- (dcmp%@%"aliases")[[expr_text(dcmp%@%"resp")]]
  structure <- dcmp%@%"seasons"
  aliases <- dcmp%@%"aliases"
  
  req_vars <- all.vars(dcmp_method)
  
  dcmp_ops <- traverse(dcmp_method,
    .f = function(x, y) c(y, x[[1]]),
    .g = function(x) x[-1],
    .h = function(x) if(is_call(x)) x[[1]] else NULL
  )
  
  if(any(map_chr(dcmp_ops, as_string) != "+")){
    abort("Only modelling of additive decompositions is supported.")
  }
  
  mdls <- dots_list(...) %>% 
    map(function(x) estimate(dcmp, x))
  
  alias_vars <- function(expr, aliases){
    vars <- all.vars(expr)
    alias_vars <- aliases[intersect(vars, names(aliases))] %>% 
      map(alias_vars, aliases = aliases) %>% 
      invoke(c, .)
    unname(c(setdiff(vars, names(aliases)), alias_vars))
  }
  
  mdl_vars <- map(mdls, `[[`, "response") %>% 
    map(alias_vars, aliases = aliases) %>% 
    invoke(c, .)
                  
  miss_vars <- setdiff(req_vars, mdl_vars)
  
  if(!all(miss_vars %in% names(structure))) {
    abort(sprintf(
"Suitable defaults for these decomposition elements are not available: %s.
Please specify an appropriate model for these components",
      paste0(setdiff(miss_vars, names(structure)), collapse = ", "))
    )
  }
  
  mdls_default <- structure[miss_vars] %>% 
    imap(function(x, nm){
      estimate(dcmp, 
        SNAIVE(new_formula(lhs = sym(nm), rhs = expr(lag(!!x[["period"]]))))
      )
    })
  
  model <- reduce(c(mdls, mdls_default), `+`)
  
  if(!isTRUE(all.equal(response(model)[[".response"]], est[[measured_vars(est)]]))){
    abort(
"The models specified do not combine to give the correct response.
Please check that you have specified the decomposition models appropriately.")
  }
  
  structure(
    list(
      est = est %>% 
        mutate(
          .fitted = fitted(model)[[".fitted"]],
          .resid = !!sym(measured_vars(est)) - !!sym(".fitted")
        ),
      fit = tibble(method = "Decomposition model",
                   decomposition = list(dcmp_method)),
      model = model
    ),
    class = "decomposition_model"
  )
}

decomposition_model <- R6::R6Class(NULL,
  inherit = model_definition,
  public = list(
    model = "decomposition",
    train = train_decomposition,
    specials = NULL,
    dcmp_fn = NULL,
    dcmp_args = NULL,
    initialize = function(.f, formula, ..., dcmp_args = list()){
      self$formula <- enquo(formula)
      self$dcmp_fn <- .f
      self$env <- caller_env(n = 2)
      self$dcmp_args <- dcmp_args
      self$extra <- list2(...)
    }
  )
)

#' @export
forecast.decomposition_model <- function(object, new_data, specials = NULL,  ...){
  fc <- forecast(object$model)
  construct_fc(fc[[expr_text(fc%@%"response")]], rep(0, NROW(fc)), fc[[expr_text(fc%@%"dist")]])
}

#' Decomposition modelling
#' 
#' @param dcmp The decomposition function (such as `STL`)
#' @param formula The formula used to describe the decomposition
#' @param ... Model definitions used to model the components (such as `ETS`)
#' @param dcmp_args Arguments to be passed to the decomposition function (`.f`)
#' 
#' @export
dcmp_model <- function(dcmp, formula, ..., dcmp_args = list()){
  decomposition_model$new(dcmp, !!enquo(formula), ..., dcmp_args = dcmp_args)
}

#' @export
fitted.decomposition_model <- function(object, ...){
  select(object$est, ".fitted")
}

#' @export
residuals.decomposition_model <- function(object, ...){
  select(object$est, ".resid")
}

#' @export
augment.decomposition_model <- function(x, ...){
  x$est
}

#' @export
glance.decomposition_model <- function(x, ...){
  x$fit
}

#' @export
tidy.decomposition_model <- function(x, ...){
  x$par
}

#' @export
model_sum.decomposition_model <- function(x){
  x$fit$method
}