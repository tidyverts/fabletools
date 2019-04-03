#' R6 Objects
#' 
#' Suitable for inheritance when creating new models or definitions.
#' 
#' @rdname definitions
#' 
#' @export
model_definition <- R6::R6Class(NULL,
  public = list(
    model = "Unknown model",
    specials = list(),
    formula = NULL,
    extra = NULL,
    env = global_env(),
    check = function(.data){
    },
    prepare = function(...){
    },
    initialize = function(formula, ..., .env){
      if(possibly(compose(is.data.frame, eval_tidy), FALSE)(self$formula)){
        abort("The API for fable models has changed. Read more here: https://github.com/tidyverts/fable/issues/77")
      }
      
      self$formula <- enquo(formula)
      
      self$.__enclos_env__ <- env_clone(self$.__enclos_env__, self$env)
      
      # Set `self` and `super` for special functions
      self$specials <- structure(as_environment(
        assign_func_envs(self$specials, self$.__enclos_env__),
        parent = self$env
      ), required_specials = self$specials%@%"required_specials")
      
      self$env <- .env
      
      self$prepare(formula, ...)
      
      self$extra <- list2(...)
    },
    data = NULL,
    add_data = function(.data){
      self$check(.data)
      self$data <- .data
    },
    remove_data = function(){
      self$data <- NULL
    },
    train = function(...){
      abort("This model has not defined a training method.")
    },
    print = function(...){
      cat("<A model definition>\n", sep = "")
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
#' [`fable::common_xregs`], an `origin` element in the model is used to store
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
      specials = specials,
      check = check,
      prepare = prepare,
      env = .env,
      ...
    ),
    parent_env = env_bury(.env, .inherit = .inherit)
  )
}

#' @rdname new-model-class
#' @param .class A model class (typically created with [new_model_class()])
#' @export
new_model_definition <- function(.class, ..., .env = caller_env(n = 2)){
  add_class(.class$new(..., .env = .env), "mdl_defn")
}

#' @rdname definitions 
#' @export
decomposition_definition <- R6::R6Class(NULL,
  public = list(
    method = "Unknown decomposition",
    train = function(...){
      abort("This decomposition has not defined a training method.")
    },
    print = function(...){
      cat("<A decomposition definition>\n", sep = "")
    }
  ),
  lock_objects = FALSE,
  inherit = model_definition
)

#' Helper to create a new decomposition function
#' 
#' @param .class A decomposition class (typically created with [new_decomposition_class()]).
#' @param .data A tsibble.
#' @param ... The user inputs, such as the formula and any control parameters.
#' @param .env The environment from which the user's objects can be found.
#' 
#' @export
new_decomposition <- function(.class, .data, ..., .env = caller_env(n = 2)){
  dcmp <- new_model_definition(.class, ..., .env = .env)
  
  kv <- key_vars(.data)
  .data <- nest(group_by(.data, !!!syms(kv)), .key = "lst_data")
  
  if(NROW(.data) == 0){
    abort("There is no data to decompose!")
  }
  
  out <- mutate(.data,
                dcmp = map(!!sym("lst_data"), function(data, dcmp){
                  estimate(data, dcmp)[["fit"]]
                }, dcmp))
  
  attrs <- combine_dcmp_attr(out[["dcmp"]])
  out <- unnest(out, !!sym("dcmp"), key = kv)
  as_dable(out, method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}


#' Create a new class of decomposition
#' 
#' Suitable for extension packages to create new decompositions for fable.
#' 
#' This function produces a new R6 decomposition definition. An understanding of R6 is
#' not required, however could be useful to provide more sophisticated model
#' interfaces. All functions have access to `self`, allowing the functions for 
#' training the model and evaluating specials to access the model class itself.
#' This can be useful to obtain elements set in the %TODO
#' 
#' @param method The name of the decomposition method
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
#' [`fable::common_xregs`], an `origin` element in the model is used to store
#' the origin for `trend()` and `fourier()` specials. To use these specials, you
#' must add an `origin` element to the object (say with `origin = NULL`).
#' @param .env The environment from which functions should inherit from.
#' @param .inherit A model class to inherit from.
#' 
#' @rdname new-dcmp-class
#' 
#' @export
new_decomposition_class <- function(method = "Unknown model", 
                            train = function(.data, formula, specials, ...) abort("This decomposition has not defined a training method."),
                            specials = new_specials(),
                            check = function(.data){if(NROW(.data)==0) abort("There is no data to decompose!")},
                            prepare = function(...){},
                            ...,
                            .env = caller_env(),
                            .inherit = decomposition_definition){
  R6::R6Class(NULL, inherit = .inherit,
              public = list(
                method = method,
                train = train,
                specials = specials,
                check = check,
                prepare = prepare,
                env = .env,
                ...
              ),
              parent_env = env_bury(.env, .inherit = .inherit)
  )
}