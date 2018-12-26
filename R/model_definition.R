#' Model definition
#' 
#' @export
model_definition <- R6::R6Class("model",
  public = list(
    model = "Unknown model",
    specials = list(),
    formula = NULL,
    extra = NULL,
    initialize = function(formula, ...){
      # Set `self` and `super` for special functions
      self$specials <- assign_func_envs(self$specials, self$.__enclos_env__)
      self$formula <- enquo(formula)
      self$extra <- list2(...)
    },
    train = function(...){
      abort("This model has not defined a training method.")
    },
    data = NULL,
    print = function(...){
      cat("<A model definition>\n", sep = "")
    }
  )
)

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
    formula <- enquo(formula)
    if(possibly(compose(is.data.frame, eval_tidy), FALSE)(formula)){
      abort("The API for fable models has changed. Read more here: https://github.com/tidyverts/fable/issues/77")
    }
    structure(
      list(
        formula = formula,
        train = train, 
        specials = specials, 
        dots = list(...)),
      class = "model_definition"
    )
  }
}