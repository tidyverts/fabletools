#' Functions associated with specifying a model


# Returns a classed vctr with inheritance for methods used to parse specification elements
#' @export
new_model_specification <- function(formula, ..., class = NULL) {
  vctrs::new_vctr(
    # Capture model 'formula'
    formula = formula,
    opts = list(...),
    class = c(class, "mdl_spec")
  )
}

parse_specials <- function(object, ...) {
  UseMethod("parse_specials")
}

parse_specials.mdl_spec <- function(object, ...) {
  
}

parse_globals <- function(object, ...) {
  UseMethod("parse_globals")
}

parse_globals.mdl_spec <- function(object, ...) {
  model_lhs <- model_lhs(object)
  
  if(is_call(model_lhs) && call_name(model_lhs) == "@"){
    return(tidyselect::eval_select(model_lhs[[3L]], model$data))
  }
  
  return(NULL)
}

parse_responses <- function(object, data, ...) {
  UseMethod("parse_globals")
}

parse_responses.mdl_spec <- function(object, data, ...) {
  model_lhs <- model_lhs(object)
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
    environment(trans) <- new_environment(data, get_env(trans))
    inv <- invert_transformation(trans)
    environment(inv) <- new_environment(data, get_env(inv))
    y <- eval_tidy(rlang::parse_expr(resp), data = data, env = model$env)
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

# Open questions / problems
# 1. How to mix global and non-global models? 
# -- specification vector is closely tied to key structure