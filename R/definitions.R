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
    initialize = function(formula, ...){
      if(possibly(compose(is.data.frame, eval_tidy), FALSE)(self$formula)){
        abort("The API for fable models has changed. Read more here: https://github.com/tidyverts/fable/issues/77")
      }
      
      self$formula <- enquo(formula)
      self$env <- caller_env(n = 2)
      
      # Set `self` and `super` for special functions
      self$specials <- structure(as_environment(
        assign_func_envs(self$specials, self$.__enclos_env__),
        parent = caller_env(2)
      ), required_specials = self$specials%@%"required_specials")
      
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
                            .inherit = model_definition){
  R6::R6Class(NULL, inherit = .inherit,
    public = list(
      model = model,
      train = train,
      specials = specials,
      check = check,
      prepare = prepare,
      ...
    )
  )
}

#' @rdname new-model-class
#' @param .class A model class (typically created with [new_model_class()])
#' @export
new_model_definition <- function(.class, ...){
  .class$new(...)
}

#' @rdname definitions 
#' @export
decomposition_definition <- R6::R6Class(NULL,
  public = list(
    method = "Unknown decomposition",
    specials = list(),
    formula = NULL,
    extra = NULL,
    env = global_env(),
    initialize = function(formula, ...){
      self$formula <- enquo(formula)
      self$env <- caller_env(n = 2)
      
      # Set `self` and `super` for special functions
      self$specials <- structure(as_environment(
        assign_func_envs(self$specials, self$.__enclos_env__),
        parent = caller_env(2)
      ), required_specials = self$specials%@%"required_specials")
      
      self$extra <- list2(...)
    },
    train = function(...){
      abort("This decomposition has not defined a training method.")
    },
    data = NULL,
    print = function(...){
      cat("<A decomposition definition>\n", sep = "")
    }
  ),
  lock_objects = FALSE
)

#' Helper to create a new decomposition function
#' 
#' @param defn R6 decomposition definition
#' 
#' @export
new_decomposition <- function(defn){
  fmls <- formals(defn$public_methods$train)
  fmls <- fmls[names(fmls) != "specials"]
  extra <- names(fmls)[!(names(fmls) %in% c(".data", "formula", "..."))]
  extra <- set_names(syms(extra), extra)
  new_function(
    fmls,
    body(function(formula, ...){
      keys <- key(.data)
      dcmp <- defn$new(!!enquo(formula), !!!extra, ...)
      fablelite::validate_formula(dcmp, .data)
      .data <- nest(group_by(.data, !!!keys), .key = "lst_data")
      
      eval_dcmp <- function(lst_data){
        map(lst_data, function(data){
          dcmp$data <- data
          parsed <- fablelite::parse_model(dcmp)
          data <- transmute(data, !!model_lhs(parsed$model))
          eval_tidy(
            expr(dcmp$train(.data = data, formula = dcmp$formula,
                            specials = parsed$specials, !!!dcmp$extra))
          )
        })
      }
      out <- mutate(.data, dcmp = eval_dcmp(!!sym("lst_data")))
      
      attrs <- combine_dcmp_attr(out[["dcmp"]])
      out <- unnest(out, !!sym("dcmp"), key = keys)
      as_dable(out, method = attrs[["method"]], resp = !!attrs[["response"]],
               seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
    }),
    env = environment()
  )
}