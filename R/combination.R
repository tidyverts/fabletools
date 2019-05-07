train_combination <- function(.data, formula, specials, ..., cmbn_fn){
  mdls <- dots_list(...) %>% 
    map(function(x) estimate(self$data, x))
  
  do.call(cmbn_fn, mdls)
}

#' Combination modelling
#' 
#' @param ... Model definitions used in the combination (such as [fable::ETS()])
#' @param cmbn_fn A function used to produce the combination
#' 
#' @export
cmbn_model <- function(..., cmbn_fn = cmbn_ensemble){
  mdls <- dots_list(...)
  if(any(!map_lgl(mdls, inherits, "mdl_defn"))){
    abort("All models provided to `cmbn_model()` must be valid model definitions.")
  }
  
  cmbn_model <- new_model_class("cmbn_mdl", train = train_combination, 
                                specials = new_specials(xreg = function(...) NULL))
  new_model_definition(cmbn_model, !!quo(!!model_lhs(mdls[[1]])), ..., 
                       cmbn_fn = cmbn_fn)
}

#' Ensemble combination
#' 
#' @param ... Estimated models used in the ensemble.
#' 
#' @export
cmbn_ensemble <- function(...){
  mdls <- dots_list(...)
  reduce(mdls, `+`)/length(mdls)
}

new_model_combination <- function(x, combination){
  mdls <- map_lgl(x, inherits, "model")
  
  mdls_response <- map(x, function(x) if(is_model(x)) x[["response"]] else x)
  comb_response <- map(transpose(mdls_response),
                       function(x) eval(expr(substitute(!!combination, x))))
  
  # Try to simplify the response
  if(any(!mdls)){
    op <- deparse(combination[[1]])
    if(op == "*" || (op == "/" && mdls[1])){
      num <- x[[which(!mdls)]]
      num <- switch(op, `*` = 1/num, `/` = num)
      if(num%%1 == 0){
        cmp <- all.names(x[[which(mdls)]][["response"]])
        if(length(unique(cmp)) == 2 && sum(cmp == "+") + 1 == num){
          comb_response <- sym(cmp[cmp != "+"][1])
        }
      }
    }
  }
  
  if(length(comb_response) > 1) abort("Combining multivariate models is not yet supported.")
  
  # Compute new response data
  resp <- map2(x, mdls, function(x, is_mdl) if(is_mdl) response(x)[[".response"]] else x)
  resp <- eval_tidy(combination, resp)
  
  new_model(
    structure(x, combination = combination, class = c("model_combination")),
    model = x[[which(mdls)[1]]][["model"]],
    data = transmute(x[[which(mdls)[1]]][["data"]], !!expr_text(comb_response[[1]]) := resp),
    response = comb_response,
    transformation = list(new_transformation(identity, identity))
  )
}

#' @export
model_sum.model_combination <- function(x){
  "COMBINATION"
}

#' @export
Ops.model <- function(e1, e2){
  e1_expr <- enexpr(e1)
  e2_expr <- enexpr(e2)
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    warn(sprintf("`%s` not meaningful for models", .Generic))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic == "/" && is_model(e2)){
    warn(sprintf("Cannot divide by a model"))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    .Generic <- "*"
    e1 <- 1
  }
  if(.Generic == "-"){
    .Generic <- "+"
    e2 <- -e2
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    e2 <- 1/e2
  }
  e_len <- c(length(e1), length(e2))
  if(max(e_len) %% min(e_len) != 0){
    warn("longer object length is not a multiple of shorter object length")
  }
  
  if(e_len[[1]] != e_len[[2]]){
    if(which.min(e_len) == 1){
      is_mdl <- is_model(e1)
      cls <- class(e1)
      e1 <- rep_len(e1, e_len[[2]])
      if(is_mdl){
        e1 <- structure(e1, class = cls)
      }
    }
    else{
      is_mdl <- is_model(e2)
      cls <- class(e2)
      e2 <- rep_len(e2, e_len[[1]])
      if(is_mdl){
        e2 <- structure(e2, class = cls)
      }
    }
  }
  
  if(is_model(e1) && is_model(e2)){
    if(.Generic == "*"){
      warn(sprintf("Multipling models is not supported"))
      return(rep.int(NA, length(e1)))
    }
  }
  
  new_model_combination(
    list(e1 = e1, e2 = e2),
    combination = call2(.Generic, sym("e1"), sym("e2"))
  )
}

#' @export
Ops.lst_mdl <- function(e1, e2){
  add_class(map2(e1, e2, .Generic), "lst_mdl")
}

#' @importFrom stats var
#' @export
forecast.model_combination <- function(object, new_data, specials, ...){
  mdls <- map_lgl(object, is_model)
  expr <- attr(object, "combination")
  # Compute residual covariance to adjust the forecast variance
  # Assumes correlation across h is identical
  
  if(all(mdls)){
    fc_cov <- var(
      residuals(object[[1]], type = "response")[[".resid"]],
      residuals(object[[2]], type = "response")[[".resid"]],
      na.rm = TRUE
    )
  }
  else{
    fc_cov <- 0
  }
  object[mdls] <- map(object[mdls], forecast, new_data = new_data, ...)
  
  get_attr_col <- function(x, col) if(is_fable(x)) x[[expr_text(attr(x, col))]] else x 
  
  # var(x) + var(y) + 2*cov(x,y)
  .dist <- eval_tidy(expr, map(object, get_attr_col, "dist"))
  .dist <- add_class(map(.dist, function(x) {x$sd <- sqrt(x$sd^2 + 2*fc_cov); x}), "fcdist")
  
  .fc <- eval_tidy(expr, map(object, function(x) 
    if(is_fable(x)) x[[expr_text(attr(x, "response")[[1]])]] else x))
  
  construct_fc(
    point = .fc,
    sd = rep(0, NROW(object[[which(mdls)[[1]]]])),
    dist = .dist
  )
}

#' @export
fitted.model_combination <- function(object, ...){
  mdls <- map_lgl(object, is_model)
  expr <- attr(object, "combination")
  object[mdls] <- map(object[mdls], fitted, ...)
  fits <- map(object, function(x) if(is_tsibble(x)) x[[".fitted"]] else x)
  eval_tidy(expr, fits)
}
