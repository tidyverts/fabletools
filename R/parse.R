parse_specials <- function(call = NULL, specials = NULL){
  if(!is.null(call)){ # No model specified
    call <- enexpr(call)
    
    # Don't parse xreg_specials - leave them to the xregs
    nm <- setdiff(names(specials), attr(specials, "xreg_specials"))
    
    parsed <- traverse_call(!!call,
                            .f = function(.x, ...) {
                              merge_named_list(.x[[1]], .x[[2]])},
                            .g = function(.x){ 
                              map(as.list(get_expr(.x))[-1], expr)
                            },
                            .h = function(x){ # Base types
                              x <- get_expr(x)
                              if(!is_call(x) || !(call_name(x) %in% nm)){
                                list(list(x))
                              }
                              else{# Current call is a special function
                                set_names(list(list(x)), call_name(x))
                              }
                            },
                            base = function(.x){
                              .x <- get_expr(.x)
                              !is_call(.x) || call_name(.x) != "+"
                            }
    )
  } else {
    parsed <- list()
  }
  
  bare_xreg <- names_no_null(parsed) == ""
  if(any(bare_xreg)){
    parsed$xreg[[length(parsed$xreg) + 1]] <- expr((!!sym("xreg"))(!!!parsed[[which(bare_xreg)]]))
    parsed[[which(bare_xreg)]] <- NULL
  }
  
  # Add required_specials
  missing_specials <- setdiff(attr(specials, "required_specials"), names(parsed))
  parsed[missing_specials] <- map(missing_specials, function(x) list(call(x)))
  
  parsed
}

#' Validate the user provided model
#' 
#' Appropriately format the user's model for evaluation. Typically ran as one of the first steps
#' in a model function.
#' @param model A quosure for the user's model specification
#' @param data A dataset used for automatic response selection
#' 
#' @export
validate_formula <- function(model, data = NULL){
  # Clean inputs
  if(!is_quosure(model$formula)){
    model$formula <- new_quosure(model$formula)
  }
  
  if(quo_is_missing(model$formula)){
    model$formula <- guess_response(data)
  }
  else{
    if(possibly(compose(is.formula, eval_tidy), FALSE)(model$formula)){
      f_env <- get_env(model$formula)
      model$formula <- eval_tidy(model$formula)
      environment(model$formula) <- f_env
      
      # Add response if missing
      if(is.null(model_lhs(model))){
        model$formula <- new_formula(
          lhs = guess_response(data),
          rhs = model_rhs(model),
          env = get_env(model$formula)
        )
      }
    }
    else{
      model$formula <- get_expr(model$formula)
    }
  }
  
  invisible(model$formula)
}

#' Parse the model specification for specials
#' 
#' Using a list of defined special functions, the user's formula specification and data
#' is parsed to extract important modelling components.
#' 
#' @param model A model definition
#' 
#' @export
parse_model <- function(model){
  # Parse model
  list2(
    model = model,
    !!!parse_model_lhs(model),
    specials = parse_model_rhs(model)
  )
}

#' Parse the RHS of the model formula for specials
#' 
#' @inheritParams parse_model
#' 
#' @export
parse_model_rhs <- function(model){
  # if(length(model$specials) == 0){
  #   return(list(specials = NULL))
  # }
  rhs <- model_rhs(model)
  specials <- parse_specials(rhs, specials = model$specials)
  map(specials, function(.x){
      map(.x, eval_tidy, data = model$data, env = model$specials)
  })
}

#' Parse the RHS of the model formula for transformations
#' 
#' @inheritParams parse_model
#' 
#' @export
parse_model_lhs <- function(model){
  model_lhs <- model_lhs(model)
  if(is_call(model_lhs) && call_name(model_lhs) == "vars"){
    model_lhs[[1]] <- sym("exprs")
    model_lhs <- eval(model_lhs)
  }
  else{
    model_lhs <- list(model_lhs)
  }
  is_resp <- function(x) is_call(x) && x[[1]] == sym("resp")
  
  # Traverse call removing all resp() usage
  # This is used to evaluate the response from the input data
  response_exprs <- lapply(model_lhs, traverse,
                           .f = function(x, y) {
                             if(is_resp(y)) x[[1]] else call2(call_name(y), !!!x)
                           },
                           .g = function(x) x[-1],
                           # .h = function(x) if(is_resp(x)) x[[length(x)]] else x,
                           base = function(x) is_syntactic_literal(x) || is_symbol(x)
  )
  
  # Traverse call to parse out AST for transformations
  traversed_lhs <- lapply(model_lhs, traverse,
     .f = function(x, y) {
       if(any(resp_pos <- map_lgl(x, function(x) any(names(x) %in% "response")))){
         if(sum(resp_pos) != 1) abort("The `resp()` function can only be used once per response variable. For multivariate modelling, use `vars()`.")
         names(x)[resp_pos] <- "response"
       }
       `attr<-`(x, "call", y[[1]])
     },
     .g = function(x) x[-1],
     .h = function(x) {
       if(is_resp(x)){
         if(length(x) > 2) abort("The response variable accepts only one input. For multivariate modelling, use `vars()`.")
         list(response = x)
       } else list(x)
     },
     base = function(x) is_syntactic_literal(x) || is_symbol(x) || is_resp(x)
  )
  
  # Reduce traversal down to the response
  # If the response is set via resp(), remove all usage of resp() from the traversal
  # If the response is not set via resp(), identify the response by the maximum length object until encountering ties
  traversed_lhs <- lapply(traversed_lhs, traverse,
    .f = function(x, y){
      # Capture parent expression of base case
      cl <- NULL
      if(length(x) == 0){
        # Multiple length `n` variables found and cannot disambiguate response
        # Start with most disaggregated result of computation as response.
        x <- if(is.null(attr(y, "call"))) list(y[[1]]) else syms(as_label(attr(y, "call")))
      }
      else{
        if(is.null(attr(y, "call"))){
          if(is_resp(x[[1]])){
            x[[1]] <- x[[1]][[2]]
          }
        }
        else{
          # Remove resp() from call
          cl <- attr(y,"call")
          if(any(names(y) == "response")){
            cl[[which(names(y) == "response")+1]] <- x[[1]][[length(x[[1]])]]
          }
        }
      }
      c(x[[1]], cl)
    },
    .g = function(x){
      if(all(names(x) != "response") && !is.null(attr(x, "call"))){
        # parent_len <- length(eval(attr(x, "call") %||% x[[1]], envir = model$data))
        len <- map_dbl(x, function(y) length(eval(attr(y, "call") %||% y[[1]], envir = model$data, enclos = model$specials)))
        if(sum(len == max(len)) == 1){
          names(x)[which.max(len)] <- "response"
        }
      }
      if("response" %in% names(x)) x["response"] else list()
    },
    base = function(x) !is.list(x)
  )
  
  # Obtain parsed out response variable
  responses <- map(traversed_lhs, function(x) x[[1]])
  responses <- map_chr(responses, as_label)
  
  # Obtain transformation expression applied to response variable
  transform_exprs <- lapply(traversed_lhs, function(x) x[[length(x)]])
  
  # Invert transformation applied to response variable
  inverse_exprs <- lapply(traversed_lhs, function(x){
    x <- rev(x)
    result <- x[[length(x)]]
    for (i in seq_len(length(x) - 1)){
      result <- undo_transformation(x[[i]], x[[i + 1]], result)
    }
    result
  })
  
  # Produce transformation class functions for bt() usage
  make_transforms <- function(exprs, responses){
    map2(exprs, responses, function(x, response){
      new_function(args = set_names(list(missing_arg()), response), x, env = model$env)
    })
  }
  transformations <- map2(
    make_transforms(transform_exprs, responses),
    make_transforms(inverse_exprs, responses),
    new_transformation
  )
  
  # Test combinations of transformations to see if they are valid
  comb_trans <- map_lgl(transformations, function(x) 
    length(all.names(body(x))) > length(all.vars(body(x))) + 1)
  map2(transformations[comb_trans], responses[comb_trans], function(trans, resp){
    dt <- model$data
    environment(trans) <- new_environment(dt, get_env(trans))
    inv <- invert_transformation(trans)
    environment(inv) <- new_environment(dt, get_env(inv))
    y <- eval_tidy(rlang::parse_expr(resp), data = dt, env = model$env)
    valid <- all.equal(y, inv(trans(y)))
    if(!isTRUE(valid)){
      abort(
"Could not identify a valid back-transformation for this transformation.
Please specify a valid form of your transformation using `new_transformation()`.")
    }
  })
  
  list(
    expressions = response_exprs,
    response = syms(responses),
    transformation = transformations
  )
}