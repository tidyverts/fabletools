globalVariables("self")

train_decomposition <- function(.data, specials, ..., dcmp){
  # Extract raw original data
  dcmp <- components(model(self$data, dcmp))
  dcmp_method <- (dcmp%@%"aliases")[[response_vars(dcmp)]]
  structure <- dcmp%@%"seasons"
  aliases <- dcmp%@%"aliases"
  method <- dcmp%@%"method"
  
  xreg_vars <- setdiff(names(self$data), names(.data))
  dcmp[xreg_vars] <- self$data[xreg_vars]
  
  req_vars <- all.vars(dcmp_method)
  
  dcmp_ops <- traverse(dcmp_method,
    .f = function(x, y) c(y, x[[1]]),
    .g = function(x) x[-1],
    .h = function(x) if(is_call(x)) x[[1]] else NULL
  )
  
  if(any(map_chr(dcmp_ops, as_string) != "+")){
    abort("Only modelling of additive decompositions is supported.")
  }
  
  dcmp <- update_tsibble(dcmp, key = NULL)
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
    squash() %>% 
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
  
  mdls_default <- if(!is_empty(miss_vars)){
    require_package("fable")
    lag <- NULL
    
    structure[miss_vars] %>% 
      imap(function(x, nm){
        estimate(dcmp, fable::SNAIVE(
          new_formula(lhs = sym(nm), rhs = expr(lag(!!x[["period"]])))
        ))
      })
  }
  else{
    list()
  }
  
  model <- reduce(c(mdls, mdls_default), `+`)
  
  if(!isTRUE(all.equal(response(model)[[".response"]], .data[[measured_vars(.data)]]))){
    abort(
"The models specified do not combine to give the correct response.
Please check that you have specified the decomposition models appropriately.")
  }
  
  structure(model[["fit"]], dcmp_method = method,
            class = union("decomposition_model", class(model[["fit"]])))
}

#' Decomposition modelling
#' 
#' This function allows you to specify a decomposition combination model using 
#' any additive decomposition. It works by first decomposing the data using the
#' decomposition method provided to `dcmp_fn` with the given formula. Secondary
#' models are used to fit each of the components from the resulting 
#' decomposition. These models are specified after the decomposition formula.
#' All non-seasonal decomposition components must be specified, and any
#' unspecified seasonal components will be forecasted using seasonal naive. 
#' These component models will be combined according to the decomposition 
#' method, giving a combination model for the response of the decomposition.
#' 
#' @param dcmp A model definition which supports extracting decomposed [`components()`].
#' @param ... Model definitions used to model the components
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE) && requireNamespace("feasts", quietly = TRUE)) {
#' library(fable)
#' library(feasts)
#' library(tsibble)
#' library(dplyr)
#' 
#' vic_food <- tsibbledata::aus_retail %>% 
#'   filter(State == "Victoria", Industry == "Food retailing")
#'   
#' # Identify an appropriate decomposition
#' vic_food %>% 
#'   model(STL(log(Turnover) ~ season(window = Inf))) %>% 
#'   components() %>% 
#'   autoplot()
#'   
#' # Use an ARIMA model to seasonally adjusted data, and SNAIVE to season_year
#' # Any model can be used, and seasonal components will default to use SNAIVE.
#' my_dcmp_spec <- decomposition_model(
#'   STL(log(Turnover) ~ season(window = Inf)),
#'   ETS(season_adjust ~ season("N")), SNAIVE(season_year)
#' )
#' 
#' vic_food %>%
#'   model(my_dcmp_spec) %>% 
#'   forecast(h="5 years") %>% 
#'   autoplot(vic_food)
#' }
#' 
#' @seealso 
#' [*Forecasting: Principles and Practice* - Forecasting Decomposition](https://otexts.com/fpp3/forecasting-decomposition.html)
#' 
#' @export
decomposition_model <- function(dcmp, ...){
  dcmp_model <- new_model_class("dcmp_mdl", train = train_decomposition, 
                                specials = new_specials(xreg = function(...) NULL))
  new_model_definition(dcmp_model, !!dcmp$formula, ..., dcmp = dcmp)
}

#' @export
model_sum.decomposition_model <- function(x){
  paste(x%@%"dcmp_method", "decomposition model")
}

#' @export
report.model_combination <- function(object, ...){
  is_mdl_cmbn <- function(x) inherits(x, "model_combination")
  comb_expr <- traverse(
    object, .f = function(resp, comb) eval(expr(substitute(!!comb, resp))),
    .h = function(x) if(is_model(x)) x[["response"]][[1]] else if (is_mdl_cmbn(x)) x%@%"combination" else x,
    base = compose(`!`, is_mdl_cmbn))
  
  cmbn <- sprintf("Combination: %s", expr_text(comb_expr))
  cat(sprintf("%s\n\n%s\n\n", cmbn, strrep("=", nchar(cmbn))))
  
  traverse(object, .h = function(x) if(is_model(x)) {report(x);cat("\n")}, base = compose(`!`, is_mdl_cmbn))
  
  invisible(object)
}
