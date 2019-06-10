context("setup-models.R")

test_specials <- new_specials(
  rnorm = function(m,s){
    stats::rnorm(NROW(self$data), m, s)
  },
  log5 = function(x){
    logb(x, base = 5)
  },
  oops = function(){
    stop("Not allowed")
  },
  xreg = function(...){
    deparse(match.call())
  }
)

test_train <- function(specials, ...) specials

no_specials <- function(formula, ...){
  no_specials_model <- new_model_class(model = "test model", train = test_train, specials = NULL)
  new_model_definition(no_specials_model, !!enquo(formula), ...)
}

specials <- function(formula, ...){
  specials_model <- new_model_class(model = "test model", train = test_train, specials = test_specials)
  new_model_definition(specials_model, !!enquo(formula), ...)
}