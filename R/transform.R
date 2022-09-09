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
          abort(sprintf("No supported inverse for the `%s` transformation.", fn))
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
#' @param bt The back-transformation function
#' @param sd The forecast standard deviation
#' 
#' @examples 
#' 
#' adj_fn <- bias_adjust(function(x) exp(x), 1:10)
#' y <- rnorm(10)
#' exp(y)
#' adj_fn(y)
#' 
#' @keywords internal
#' @export
bias_adjust <- function(bt, sd){
  fvar <- sd^2
  if(any(is.na(fvar))){
    warn("Could not bias adjust the point forecasts as the forecast standard deviation is unknown. Perhaps your series is too short or insufficient bootstrap samples are used.")
    return(bt)
  }
  if(is.name(body(bt))){
    return(bt)
  }
  function(x){
    h <- .Machine$double.eps^(1/4)
    f0 <- bt(x)
    
    # Set up hessian function
    .x_var <- sym(names(formals(bt)))
    bt_hessian <- bt
    
    # Try for symbolic derivatives
    body(bt_hessian) <- possibly(stats::deriv, NULL)(
      body(bt), names(formals(bt))[1], hessian = TRUE)
    
    # Fall back to numerical derivatives
    if(is.null(body(bt_hessian))){
      body(bt_hessian) <- expr({
        h <- abs(1e-4 * !!.x_var) + 1e-4 * (abs(!!.x_var) < sqrt(.Machine$double.eps/7e-07))
        structure(list(), hessian = (bt(!!.x_var + h) - 2 * f0 + bt(!!.x_var - h))/h^2)
      })
      environment(bt_hessian) <- env_bury(get_env(bt), bt = bt, f0 = f0, h = h)
    }
    
    hessian <- as.numeric(attr(bt_hessian(x), "hessian"))

    if(any(!is.na(f0) & !is.finite(hessian))){
      warn("Could not bias adjust the point forecasts as the back-transformation's hessian is not well behaved. Consider using a different transformation.")
      return(f0)
    }
    f0 + fvar/2*hessian
  }
}

#' @export
print.transformation <- function(x, ...){
  cat("Transformation: ", expr_text(body(x)), "\n",
      "Backtransformation: ", expr_text(body(x%@%"inverse")), sep="")
}

#' @rdname new_transformation
#' @param x A transformation (such as one created with `new_transformation`).
#' @param ... Further arguments passed to other methods.
#' @export
invert_transformation <- function(x, ...){
  UseMethod("invert_transformation")
}

#' @export
invert_transformation.transformation <- function(x, ...){
  new_transformation(attr(x, "inverse"), `attributes<-`(x, NULL))
}

inverse_table <- inverse_table()

map(c("log", "logb"),
    function(.x) inverse_table$add("base", .x, 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      expr(exp(!!result))
                    }
                    else{
                      expr((!!args[[2]]) ^ !!result)
                    }
                  }
    )
)

inverse_table$add("base", "log10",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(10^!!result)
                  }
)

inverse_table$add("base", "log2",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(2^!!result)
                  }
)

inverse_table$add("base", "log1p",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(expm1(!!result))
                  }
)

inverse_table$add("base", "expm1",
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(log1p(!!result))
                  }
)

inverse_table$add("base", "exp", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(log(!!!replace(args, target_pos, list(result))))
                  }
)

inverse_table$add("fabletools", "box_cox", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(inv_box_cox(!!!replace(args, target_pos, list(result))))
                  }
)

inverse_table$add("fabletools", "inv_box_cox", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(box_cox(!!!replace(args, target_pos, list(result))))
                  }
)

inverse_table$add("base", "sqrt", 
                  function(operation, target, result){
                    expr((!!result) ^ 2)
                  }
)

inverse_table$add("base", "^", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(target_pos == 1){
                      if(args[[2]] == 0){
                        abort(sprintf("Cannot invert %s.", expr_text(operation)))
                      }
                      expr((!!result) ^ (1 / !!args[[2]]))
                    }
                    else{
                      expr(log(!!result) / log(!!args[[1]]))
                    }
                  }
)

inverse_table$add("base", "+", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      result
                    }
                    else{
                      expr(!!result - !!args[[-target_pos]])
                    }
                  }
)

inverse_table$add("base", "-", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(length(args) == 1){
                      expr(-!!result)
                    }
                    else{
                      if(target_pos == 1){
                        expr(!!result + !!args[[2]])
                      }
                      else{
                        expr(!!args[[1]] - !!result)
                      }
                    }
                  }
)

inverse_table$add("base", "/", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    if(target_pos == 1){
                      expr(!!args[[2]] * !!result)
                    }
                    else{
                      expr(!!args[[1]] / !!result)
                    }
                  }
)

inverse_table$add("base", "*", 
                  function(operation, target, result){
                    args <- call_args(operation)
                    target_pos <- match(list(target), args)
                    expr(!!result / !!args[[-target_pos]])
                  }
)

inverse_table$add("base", "(", 
                  function(operation, target, result){
                    call2("(", !!!exprs(!!result))
                  }
)