#' R6 Objects
#' 
#' Suitable for inheritance when creating new models or definitions.
#' 
#' @rdname definitions
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
  ),
  lock_objects = FALSE
)

#' @rdname definitions 
#' @export
decomposition_definition <- R6::R6Class("decomposition",
  public = list(
    method = "Unknown decomposition",
    specials = list(),
    formula = NULL,
    extra = NULL,
    initialize = function(formula, ...){
      self$formula <- enquo(formula)
      
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
      as_dable(out, resp = !!attrs[["response"]], dcmp = !!attrs[["decomposition"]],
               structure = attrs[["structure"]])
    }),
    env = environment()
  )
}