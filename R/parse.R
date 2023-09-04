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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
  response_exprs <- lapply(
    model_lhs, traverse,
    .f = function(x, y) {
      if(is_resp(y)) x[[1]] else call2(y[[1]], !!!x)
    },
    .g = function(x) x[-1],
    # .h = function(x) if(is_resp(x)) x[[length(x)]] else x,
    base = function(x) is_syntactic_literal(x) || is_symbol(x)
  )
  
  has_resp <- function(x) traverse(
    x,
    .f = function(x,y) x[[1]]||y, .g = function(x) x[-1], .h = is_resp,
    base = function(x) is_syntactic_literal(x) || is_symbol(x)
  )
  
  # Traverse call AST to parse out order of transformations
  #
  # If the response is set via resp(), remove all usage of resp() from the traversal
  # If the response is not set via resp(), identify the response by the maximum length object until encountering ties
  # 
  # Returns a list of increasing depth of transformations
  traversed_lhs <- lapply(model_lhs, 
    function(x) {
      len1vals <- list()
      path <- traverse(
        x,
        .f = function(x, y) {
          # Special handling for if the response was found by a length tie
          if((length(x[[1]]$response == 1L) && (as_label(x[[1]]$response) == as_label(y[[1]])))) {
            return(x[[1]])
          }
          
          # Rebuild the expression
          args <- lapply(x, function(y) {
            y[[length(y)]]
          })
          y <- as.call(c(y[[1]][[1]], args))
          
          # Search for response
          path <- compact(lapply(x, `[[`, "response"))
          if(length(path) > 0) return(list(response = c(path[[1]], y)))
          
          # Otherwise keep the path that isn't length 1
          path <- x[[which(map_lgl(x, function(x) is.name(x[[1]]) && !(as_label(x[[1]]) %in% names(len1vals))))]]
          c(path, y)
        },
        .g = function(x) {
          # traverse only call arguments
          args <- x[-1]
          
          # search for resp() to avoid unneeded evaluation
          resp_loc <- which(map_lgl(args, has_resp))
          if(length(resp_loc) > 1) {
            abort("The `resp()` function can only be used once per response variable. For multivariate modelling, use `vars()`.")
          }
          non_resp <- if(length(resp_loc) == 0) seq_along(args) else -resp_loc
          
          res <- map(args[non_resp], function(y) eval(y, envir = model$data, enclos = model$specials))
          len <- map_dbl(res, length)
          
          if(length(unique(len[len!=1])) > 1){
            abort(
              sprintf(
                "Response variable transformation has incompatible lengths, all arguments must be the length of the data %i or 1.",
                max(len)
              )
            )
          }
          
          # store length 1 arguments for transformation environment
          len1check <- function(len, arg) {
            (len == 1) && (is.name(arg) || (is.call(arg) && !(as_label(arg[[1]]) %in% "length")))
          }
          
          if(any(is_singular <- map2_lgl(len, args[non_resp], len1check))) {
            nm <- map_chr(args[non_resp][is_singular], as_label)
            args[non_resp][is_singular] <- syms(nm)
            len1vals[nm] <<- res[is_singular]
          }
          
          # handle unspecified response with equal length args
          if((length(resp_loc) == 0) && (sum(len == max(len)) > 1)) {
            return(list(call("resp", x)))
          }
          
          args
        },
        .h = function(x) {
          if(is_resp(x)){
            if(length(x[-1]) > 2) abort("The response variable accepts only one input. For multivariate modelling, use `vars()`.")
            list(response = sym(as_label(x[[2]])))
          } else list(x)
        },
        base = function(x) {
          is_syntactic_literal(x) || is_symbol(x) || is_resp(x)
        }
      )
      if("response" %in% names(path)) path <- path$response
      if(!is.list(path)) path <- list(path)
      list(path = path, len1vals = len1vals)
    }
  )
  
  # Obtain parsed out response variable
  responses <- map(traversed_lhs, function(x) x$path[[1]])
  responses <- map_chr(responses, as_label)
  
  # Obtain transformation expression applied to response variable
  transform_exprs <- lapply(traversed_lhs, function(x) x$path[[length(x$path)]])
  
  # Invert transformation applied to response variable
  inverse_exprs <- lapply(traversed_lhs, function(x){
    x <- rev(x$path)
    result <- x[[length(x)]]
    for (i in seq_len(length(x) - 1)){
      result <- undo_transformation(x[[i]], x[[i + 1]], result)
    }
    result
  })
  
  # Create evaluation environment for transformation functions
  # Includes cached values of single length arguments
  transform_args <- lapply(traversed_lhs, `[[`, "len1vals")
  envs <- lapply(transform_args, new_environment,  parent = model$env)
  
  # Produce transformation class functions for bt() usage
  make_transforms <- function(exprs, responses, envs){
    .mapply(
      function(x, response, env){
        new_function(args = set_names(list(missing_arg()), response), x, env = env)
      },
      dots = list(x = exprs, response = responses, env = envs),
      MoreArgs = list()
    )
  }
  
  transformations <- map2(
    make_transforms(transform_exprs, responses, envs),
    make_transforms(inverse_exprs, responses, envs),
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