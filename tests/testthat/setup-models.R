context("setup-models.R")

test_specials <- new_specials(
  rnorm = function(m,s){
    rnorm(NROW(self$data), m, s)
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

no_specials_model <- R6::R6Class("test",
                                 inherit = model_definition,
                                 public = list(
                                   model = "test model",
                                   train = test_train,
                                   specials = NULL
                                 )
)

no_specials <- no_specials_model$new

specials_model <- R6::R6Class("test",
                              inherit = model_definition,
                              public = list(
                                model = "test model",
                                train = test_train,
                                specials = test_specials
                              )
)

specials <- specials_model$new