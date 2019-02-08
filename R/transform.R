# Lookup table for function inverses
inverse_table <- function() {
  table <- new.env(parent = emptyenv())
  list(
    add = function(ns, fn, inv) {
      table[[ns]] <- as.list(table[[ns]])
      table[[ns]][[fn]] <- inv
    },
    
    get = function(ns, fn) {
      ns_name <- environmentName(ns)
      if(nchar(ns_name) == 0){
        ns_name <- "base"
      }
      ret <- table[[ns_name]][[fn]]
      if (is.null(ret)) {
        t_fn <- get(fn, envir = ns)
        if(inherits(t_fn, "transformation")){
          ret <- function(operation, target, result){
            args <- call_args(operation)
            target_pos <- match(list(target), args)
            call2(expr(invert_transformation(!!t_fn)), !!!replace(args, target_pos, list(result)))
          }
        }
        else{
          abort("No supported inverse for this function")
        }
      }
      ret
    })
}

undo_transformation <- function(operation, target, result){
  fn <- call_name(operation)
  env <- get_env(operation, caller_env())
  ns <- eval_tidy(expr(environment(get(!!fn))), env = env)
  inverse_table$get(ns, fn)(operation, get_expr(target), result)
}

traverse_transformation <- function(transformation){
  transformation <- enquo(transformation)
  
  # Evaluate transformation stack via call traversal along longest argument (hopefully, the data)
  traverse_call(!!transformation,
                .f = function(.x, .y) append(.y, .x[[1]]),
                .g = function(.x) {
                  .x %>%
                  get_expr %>%
                  as.list %>% 
                  map(new_quosure, env = get_env(.x)) %>%
                  .[which.max(map(., function(.x) length(eval_tidy(.x))))]
                },
                .h = list)
}

#' Create a new modelling transformation
#' 
#' Produces a new transformation for fable modelling functions which will be used to transform, back-transform, and adjust forecasts.
#' 
#' For more details about transformations, read the vignette:
#' `vignette("transformations", package = "fable")`
#' 
#' @param transformation A function which transforms the data
#' @param inverse A function which is the inverse of a transformation
#' 
#' @examples
#' 
#' scaled_logit <- function(x, lower=0, upper=1){
#'   log((x-lower)/(upper-x))
#' }
#' inv_scaled_logit <- function(x, lower=0, upper=1){
#'   (upper-lower)*exp(x)/(1+exp(x)) + lower
#' }
#' my_scaled_logit <- new_transformation(scaled_logit, inv_scaled_logit)
#' 
#' t_vals <- my_scaled_logit(1:10, 0, 100)
#' t_vals
#' 
#' @export
new_transformation <- function(transformation, inverse){
  structure(transformation, class = "transformation", inverse = inverse)
}

#' Bias adjust back-transformation functions
#' 
#' To produce forecast means (instead of forecast medians) it is necessary to adjust the back-transformation function relative to the forecast variance.
#' 
#' More details about bias adjustment can be found in the transformations vignette: read the vignette:
#' `vignette("transformations", package = "fable")`
#' 
#' @param bt_fn The back-transformation function
#' @param fvar The forecast variance
#' 
#' @examples 
#' 
#' adj_fn <- bias_adjust(exp, 1:10)
#' y <- rnorm(10)
#' exp(y)
#' adj_fn(y)
#' 
#' @importFrom numDeriv hessian
#' @export
bias_adjust <- function(bt_fn, fvar){
  new_function(alist(x=), expr((!!bt_fn)(!!sym("x")) + !!fvar/2*map_dbl(as.numeric(!!sym("x")), hessian, func = !!bt_fn)))
}

#' @export
print.transformation <- function(x, ...){
  cat("Transformation: ", expr_text(body(x)), "\n",
      "Backtransformation: ", expr_text(body(x%@%"inverse")), sep="")
}

invert_transformation <- function(x, ...){
  UseMethod("invert_transformation")
}

invert_transformation.transformation <- function(x, ...){
  new_transformation(x%@%"inverse", `attributes<-`(x, NULL))
}

inverse_table <- inverse_table()

map(c("log", "logb"),
    function(.x) inverse_table$add("base", .x, 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      call2("exp", !!!replace(args, target_pos, list(result)))
                    }
                    else{
                      call2("^", !!!exprs(!!args[[-target_pos]], !!result))
                    }
                  }
    )
)

inverse_table$add("base", "log10",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("^", !!!exprs(10, !!result))
                  }
)

inverse_table$add("base", "log2",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("^", !!!exprs(2, !!result))
                  }
)

inverse_table$add("base", "log1p",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("expm1", !!!exprs(!!result))
                  }
)

inverse_table$add("base", "exp", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("log", !!!replace(args, target_pos, list(result)))
                  }
)

inverse_table$add("fable", "BoxCox", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("InvBoxCox", !!!replace(args, target_pos, list(result)))
                  }
)

inverse_table$add("fable", "InvBoxCox", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("BoxCox", !!!replace(args, target_pos, list(result)))
                  }
)

inverse_table$add("base", "sqrt", 
                  function(operation, target, result){
                    call2("^", !!!exprs(!!result, 2))
                  }
)

inverse_table$add("base", "^", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("^", !!!exprs(!!result, !!call2("/", !!!exprs(1, !!args[[2]]))))
                  }
)

inverse_table$add("base", "+", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      call2("+", !!!exprs(!!result))
                    }
                    else{
                      call2("-", !!!exprs(!!result, !!args[[-target_pos]]))
                    }
                  }
)

inverse_table$add("base", "-", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      call2("-", !!!exprs(!!result))
                    }
                    else{
                      if(target_pos == 1){
                        call2("+", !!!exprs(!!result, !!args[[2]]))
                      }
                      else{
                        call2("-", !!!exprs(!!args[[1]], !!result))
                      }
                    }
                  }
)

inverse_table$add("base", "/", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(target_pos == 1){
                      call2("*", !!!exprs(!!args[[2]], !!result))
                    }
                    else{
                      call2("/", !!!exprs(!!args[[1]], !!result))
                    }
                  }
)

inverse_table$add("base", "*", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    call2("/", !!!exprs(!!result, !!args[[-target_pos]]))
                  }
)

inverse_table$add("base", "(", 
                  function(operation, target, result){
                    call2("(", !!!exprs(!!result))
                  }
)