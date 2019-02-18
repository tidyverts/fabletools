new_model_combination <- function(x, combination){
  structure(x, combination = combination, class = c("model_combination", "model"))
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

#' @importFrom stats cov
#' @export
forecast.model_combination <- function(object, ...){
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
  
  object[mdls] <- map(object[mdls], forecast, ...)
  get_attr_col <- function(x, col) if(is_fable(x)) x[[expr_text(attr(x, col))]] else x 
  fbl <- object[[which(mdls)[[1]]]] 
  fbl[[expr_text(attr(fbl, "dist"))]] <- eval_tidy(expr, map(object, get_attr_col, "dist")) + dist_normal(0, sqrt(2*fc_cov))
  fbl[[expr_text(attr(fbl, "response"))]] <- eval_tidy(expr, map(object, get_attr_col, "response"))
  fbl
}

#' @export
fitted.model_combination <- function(object, ...){
  mdls <- map_lgl(object, is_model)
  expr <- attr(object, "combination")
  object[mdls] <- map(object[mdls], fitted, ...)
  fits <- map(object, function(x) if(is_tsibble(x)) x[[".fitted"]] else x)
  out <- object[[which(mdls)[[1]]]]
  out[[".fitted"]] <- eval_tidy(expr, fits)
  out
}

#' @export
response.model_combination <- function(object, ...){
  mdls <- map_lgl(object, is_model)
  expr <- attr(object, "combination")
  object[mdls] <- map(object[mdls], response, ...)
  resp <- map(object, function(x) if(is_tsibble(x)) x[[".response"]] else x)
  out <- object[[which(mdls)[[1]]]]
  out[[".response"]] <- eval_tidy(expr, resp)
  out
}

#' @export
residuals.model_combination <- function(object, type = "response", ...){
  if(type != "response"){
    warn(sprintf('Residuals of type `%s` are not supported for model combinations.
                 Defaulting to `type="response"`', type))
    type <- "response"
  }
  resp <- response(object)
  fit <- fitted(object)
  transmute(resp, .resid = resp[[measured_vars(resp)]] - fit[[".fitted"]])
}