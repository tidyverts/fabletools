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
      self$specials <- bind_model_specials(self, self$specials, .env)
      
      self$prepare(formula, ...)
      
      self$extra <- list2(...)
    },
    recall_lag = function(x, n = 1L, ...){
      start <- NULL
      if(self$stage %in% c("generate", "forecast", "stream")){
        # Fits from fabletools <= 0.8.0 don't keep their own recent data
        if(is.null(self$recent_data)) {
          abort(c(
            "The recent data used by `lag()` isn't available for this model, as it was estimated with fabletools 0.8.0 or earlier.",
            i = "Re-estimate the model with `model()` to use it with lagged variables."
          ), call = NULL)
        }
        x_quo <- enquo(x)
        start <- eval_tidy(get_expr(x_quo), self$recent_data)
        # Variables only used through lag() needn't be in the future data (#318),
        # treat them as missing and rely on the short term memory.
        missing_vars <- setdiff(
          intersect(all.vars(get_expr(x_quo)), names(self$recent_data)),
          names(self$data)
        )
        if(length(missing_vars) > 0){
          x <- eval_tidy(x_quo, lapply(
            self$recent_data[missing_vars], vec_init, n = NROW(self$data)
          ))
        }
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
      if(is.null(self$origin) && NROW(.data) > 0) {
        self$origin <- .data[[index_var(.data)]][1]
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

# Bind a model definition's specials to `self`, scoped within `env`
bind_model_specials <- function(self, specials, env){
  # Create specials environment with user's scoping
  specials_env <- new_environment(parent = env)
  # Set `self` `super`, and `specials` in eval env for special functions
  fn_env <- new_environment(as.list(self$.__enclos_env__), self$env)
  env_bind(specials_env, !!!assign_func_envs(as.list(specials), fn_env))
  specials_env <- structure(
    specials_env,
    required_specials = specials%@%"required_specials",
    xreg_specials = specials%@%"xreg_specials"
  )
  
  # Define custom lag() xreg special for short term memory
  xreg_env <- get_env(specials_env$xreg)
  xreg_env$lag <- self$recall_lag
  specials_env
}

# Copy a model definition, with specials bound to the copy and scoped within `env`
clone_model_definition <- function(x, env = parent.env(x$specials)){
  out <- x$clone()
  out$specials <- bind_model_specials(out, x$specials, env)
  attributes(out) <- attributes(x)
  out
}

# Snapshot the global variables used by a model's formula, as the global
# environment isn't available to future workers (#412)
snapshot_model_globals <- function(x, data_cols = character()){
  f_env <- get_env(x$formula)
  if(!identical(f_env, global_env())) return(x)
  used <- setdiff(all.names(get_expr(x$formula)), data_cols)
  used <- used[map_lgl(used, exists, envir = f_env, inherits = FALSE)]
  if(is_empty(used)) return(x)
  env <- new_environment(mget(used, envir = f_env), parent = f_env)
  x <- clone_model_definition(x, env)
  x$formula <- set_env(x$formula, env)
  x
}

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
#' the origin for the `trend()` special. To use this special, you
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
      specials = specials %||% new_specials(),
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
  add_class(.class$new({{formula}}, ..., .env = .env), c("mdl_spec", "mdl_defn"))
}
