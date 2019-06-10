parse_specials <- function(call = NULL, specials = NULL){
  if(!is.null(call)){ # No model specified
    call <- enexpr(call)
    
    # Don't parse xreg_specials - leave them to the xregs
    nm <- setdiff(names(specials), specials%@%"xreg_specials")
    
    parsed <- traverse_call(!!call,
                            .f = function(.x, ...) {
                              merge_named_list(.x[[1]], .x[[2]])},
                            .g = function(.x){ 
                              .x %>%
                                get_expr %>% # Extract call
                                as.list %>% # Split call components
                                .[-1] %>% # Drop function operator (as it is known to be "+")
                                map(expr) # Rebuild quosure for recursive map
                            },
                            .h = function(x){ # Base types
                              x <- get_expr(x)
                              if(!is_call(x) || !(call_name(x) %in% nm)){
                                list(xreg = list(x))
                              }
                              else{# Current call is a special function
                                list(list(x)) %>% set_names(call_name(x))
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
  
  # Wrap xreg inputs into xreg()
  if(!is.null(parsed$xreg)){
    parsed$xreg <- list(call2("xreg", !!!parsed$xreg))
  }
  
  # Add required_specials
  missing_specials <- setdiff(attr(specials, "required_specials"), names(parsed))
  parsed[missing_specials] <- map(missing_specials, compose("list", "call2"))
  
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
    if(possibly(compose(is_formula, eval_tidy), FALSE)(model$formula)){
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
    !!!parse_model_rhs(model)
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
  list(
    specials = map(specials, function(.x){
      map(.x, eval_tidy, data = model$data, env = model$specials)
    })
  )
}

#' Parse the RHS of the model formula for transformations
#' 
#' @inheritParams parse_model
#' 
#' @export
parse_model_lhs <- function(model){
  model_lhs <- model_lhs(model)
  
  if(is_call(model_lhs) && model_lhs[[1]] == sym("vars")){
    model_lhs[[1]] <- sym("exprs")
    model_lhs <- eval(model_lhs)
  }
  else{
    model_lhs <- list(model_lhs)
  }
  is_resp <- function(x) is_call(x) && x[[1]] == sym("resp")
  
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
        x <- list(attr(y, "call") %||% y[[1]])
      }
      else{
        if(is.null(attr(y, "call"))){
          if(is_resp(x[[1]])){
            x[[1]] <- x[[1]][[2]]
          }
        }
        else{
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
        len <- map_dbl(x, function(y) length(eval(attr(y, "call") %||% y[[1]], envir = model$data, enclos = model$env)))
        if(sum(len == max(len)) == 1){
          names(x)[which.max(len)] <- "response"
        }
      }
      if("response" %in% names(x)) x["response"] else list()
    },
    base = function(x) !is.list(x)
  )
  
  responses <- map(traversed_lhs, function(x) x[[1]])
  
  transform_exprs <- lapply(traversed_lhs, function(x) x[[length(x)]])
  
  inverse_exprs <- lapply(traversed_lhs, function(x){
    x <- rev(x)
    result <- x[[length(x)]]
    for (i in seq_len(length(x) - 1)){
      result <- undo_transformation(x[[i]], x[[i + 1]], result)
    }
    result
  })
  
  make_transforms <- function(exprs, responses){
    map2(exprs, responses, function(x, response){
      x <- traverse(x,
               .f = function(x, y) as.call(c(y[[1]], x)),
               .g = function(x) x[-1],
               .h = function(x) {if(x == response) sym(".x") else x},
               base = function(x) is_syntactic_literal(x) || is_symbol(x) || x == response
      )
      new_function(args = alist(.x = ), x, env = model$env)
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
    
    valid <- all.equal(dt[[expr_text(resp)]], inv(trans(dt[[expr_text(resp)]])))
    if(!isTRUE(valid)){
      abort(
"Could not identify a valid back-transformation for this transformation.
Please specify a valid form of your transformation using `new_transformation()`.")
    }
  })
  
  list(
    expressions = transform_exprs,
    response = responses,
    transformation = transformations
  )
}