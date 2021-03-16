model_definition <- R6::R6Class(NULL,
  public = list(
    model = "Unknown model",
    specials = list(),
    formula = NULL,
    extra = NULL,
    origin = NULL,
    env = global_env(),
    check = function(.data){
    },
    prepare = function(...){
    },
    initialize = function(formula, ..., .env){
      self$formula <- enquo(formula)
      
      # self$env <- .env
      # Create specials environment with user's scoping
      specials_env <- new_environment(parent = .env)
      # Set `self` `super`, and `specials` in eval env for special functions
      fn_env <- new_environment(as.list(self$.__enclos_env__), self$env)
      env_bind(specials_env, !!!assign_func_envs(self$specials, fn_env))
      self$specials <- structure(
        specials_env,
        required_specials = self$specials%@%"required_specials",
        xreg_specials = self$specials%@%"xreg_specials"
      )
      
      # Define custom lag() xreg special for short term memory
      xreg_env <- get_env(self$specials$xreg)
      xreg_env$lag <- self$recall_lag
      
      self$prepare(formula, ...)
      
      self$extra <- list2(...)
    },
    recall_lag = function(x, n = 1L, ...){
      start <- NULL
      if(self$stage %in% c("generate", "forecast")){
        x_expr <- enexpr(x)
        start <- eval_tidy(x_expr, self$recent_data)
      }
      else if(self$stage == "estimate" && NROW(self$recent_data) < n){
        self$recent_data <- self$data[NROW(self$data) - n + seq_len(n),]
      }
      dplyr::lag(c(start, x), n = n, ...)[seq_along(x) + length(start)]
    },
    recent_data = NULL, # Used in short term memory for lagged operators
    stage = NULL, # Identifies the current operation
    data = NULL,
    add_data = function(.data){
      self$check(.data)
      # Add data origin if not yet known (fitting model)
      if(is.null(self$origin)) {
        self$origin <- .data[[index_var(.data)]][[1]]
      }
      self$data <- .data
    },
    remove_data = function(){
      self$data <- NULL
    },
    train = function(...){
      abort("This model has not defined a training method.")
    },
    print = function(...){
      cat(sprintf("<%s model definition>\n", self$model), sep = "")
    }
  ),
  lock_objects = FALSE
)

#' Create a new class of models
#' 
#' Suitable for extension packages to create new models for fable.
#' 
#' This function produces a new R6 model definition. An understanding of R6 is
#' not required, however could be useful to provide more sophisticated model
#' interfaces. All functions have access to `self`, allowing the functions for 
#' training the model and evaluating specials to access the model class itself.
#' This can be useful to obtain elements set in the %TODO
#' 
#' @param model The name of the model
#' @param train A function that trains the model to a dataset. `.data` is a tsibble
#' containing the data's index and response variables only. `formula` is the 
#' user's provided formula. `specials` is the evaluated specials used in the formula.
#' @param specials Special functions produced using [new_specials()]
#' @param check A function that is used to check the data for suitability with 
#' the model. This can be used to check for missing values (both implicit and 
#' explicit), regularity of observations, ordered time index, and univariate
#' responses.
#' @param prepare This allows you to modify the model class according to user
#' inputs. `...` is the arguments passed to `new_model_definition`, allowing
#' you to perform different checks or training procedures according to different
#' user inputs.
#' @param ... Further arguments to [R6::R6Class()]. This can be useful to set up
#' additional elements used in the other functions. For example, to use 
#' `common_xregs`, an `origin` element in the model is used to store
#' the origin for `trend()` and `fourier()` specials. To use these specials, you
#' must add an `origin` element to the object (say with `origin = NULL`).
#' @param .env The environment from which functions should inherit from.
#' @param .inherit A model class to inherit from.
#' 
#' @rdname new-model-class
#' 
#' @export
new_model_class <- function(model = "Unknown model", 
                            train = function(.data, formula, specials, ...) abort("This model has not defined a training method."),
                            specials = new_specials(),
                            check = function(.data){},
                            prepare = function(...){},
                            ...,
                            .env = caller_env(),
                            .inherit = model_definition){
  R6::R6Class(NULL, inherit = .inherit,
    public = list(
      model = model,
      train = train,
      specials = specials%||%new_specials(),
      check = check,
      prepare = prepare,
      env = .env,
      ...
    ),
    parent_env = env_bury(.env, .inherit = .inherit)
  )
}

#' @rdname new-model-class
#' @param formula The user's model formula.
#' @param .class A model class (typically created with [new_model_class()]).
#' @export
new_model_definition <- function(.class, formula, ..., .env = caller_env(n = 2)){
  add_class(.class$new({{formula}}, ..., .env = .env), "mdl_defn")
}
