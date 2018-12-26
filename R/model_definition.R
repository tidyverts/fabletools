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
      self$formula <- enquo(formula)
      if(possibly(compose(is.data.frame, eval_tidy), FALSE)(self$formula)){
        abort("The API for fable models has changed. Read more here: https://github.com/tidyverts/fable/issues/77")
      }
      
      # Set `self` and `super` for special functions
      self$specials <- structure(as_environment(
        assign_func_envs(self$specials, self$.__enclos_env__),
        parent = caller_env(2)
      ), required_specials = self$specials%@%"required_specials")
      
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