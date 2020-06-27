train_combination <- function(.data, specials, ..., cmbn_fn, cmbn_args){
  mdls <- dots_list(...)
  
  # Estimate model definitions
  mdl_def <- map_lgl(mdls, inherits, "mdl_defn")
  mdls[mdl_def] <- mdls[mdl_def] %>% 
    map(function(x) estimate(self$data, x))
  
  do.call(cmbn_fn, c(mdls, cmbn_args))[["fit"]]
}

#' Combination modelling
#' 
#' Combines multiple model definitions (passed via `...`) to produce a model
#' combination definition using some combination function (`cmbn_fn`). Currently
#' distributional forecasts are only supported for models producing normally
#' distributed forecasts.
#' 
#' A combination model can also be produced using mathematical operations.
#'
#' @param ... Model definitions used in the combination.
#' @param cmbn_fn A function used to produce the combination.
#' @param cmbn_args Additional arguments passed to `cmbn_fn`.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibble)
#' library(tsibbledata)
#' 
#' # cmbn1 and cmbn2 are equivalent and equally weighted.
#' aus_production %>%
#'   model(
#'     cmbn1 = combination_model(SNAIVE(Beer), TSLM(Beer ~ trend() + season())),
#'     cmbn2 = (SNAIVE(Beer) + TSLM(Beer ~ trend() + season()))/2
#'   )
#' 
#' # An inverse variance weighted ensemble.
#' aus_production %>%
#'   model(
#'     cmbn1 = combination_model(
#'       SNAIVE(Beer), TSLM(Beer ~ trend() + season()), 
#'       cmbn_args = list(weights = "inv_var")
#'     )
#'   )
#' }
#' @export
combination_model <- function(..., cmbn_fn = combination_ensemble,
                              cmbn_args = list()){
  mdls <- dots_list(...)
  if(!any(map_lgl(mdls, inherits, "mdl_defn"))){
    abort("`combination_model()` must contain at least one valid model definition.")
  }
  
  cmbn_model <- new_model_class("cmbn_mdl", train = train_combination, 
                                specials = new_specials(xreg = function(...) NULL))
  new_model_definition(cmbn_model, !!quo(!!model_lhs(mdls[[1]])), ..., 
                       cmbn_fn = cmbn_fn, cmbn_args = cmbn_args)
}

#' Ensemble combination
#' 
#' @param ... Estimated models used in the ensemble.
#' @param weights The method used to weight each model in the ensemble.
#' 
#' @export
combination_ensemble <- function(..., weights = c("equal", "inv_var")){
  mdls <- dots_list(...)
  
  if(all(map_lgl(mdls, inherits, "mdl_defn"))){
    return(combination_model(..., cmbn_args = list(weights = weights)))
  }
  
  weights <- match.arg(weights)
  
  if(weights == "equal"){
    out <- reduce(mdls, `+`)/length(mdls)
  }
  else if(weights == "inv_var") {
    out <- if(is_model(mdls[[1]])) list(mdls) else transpose(mdls)
    out <- map(out, function(x){
      inv_var <- map_dbl(x, function(x) 1/var(residuals(x)$.resid, na.rm = TRUE))
      weights <- inv_var/sum(inv_var)
      reduce(map2(weights, x, `*`), `+`)
    })
    if(is_model(mdls[[1]])) out <- out[[1]]
  }
  
  if(is_model(out)){
    out$response <- mdls[[1]]$response
  }
  else{
    out <- structure(
      map(out, `[[<-`, "response", mdls[[1]][[1]]$response), 
      class = c("lst_mdl", "list"))
  }
  out
}

new_model_combination <- function(x, combination){
  mdls <- map_lgl(x, is_model)
  
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
        cmp <- all.names(x[[which(mdls)]][["response"]][[1]])
        if(length(unique(cmp)) == 2 && sum(cmp == "+") + 1 == num){
          comb_response <- syms(cmp[cmp != "+"][1])
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
    data = transmute(x[[which(mdls)[1]]][["data"]], !!expr_name(comb_response[[1]]) := resp),
    response = comb_response,
    transformation = list(new_transformation(identity, identity))
  )
}

#' @export
model_sum.model_combination <- function(x){
  "COMBINATION"
}

#' @export
Ops.mdl_defn <- function(e1, e2){
  e1_expr <- enexpr(e1)
  e2_expr <- enexpr(e2)
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    warn(sprintf("`%s` not meaningful for model definitions", .Generic))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic == "/" && is_model(e2)){
    warn(sprintf("Cannot divide by a model definition"))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  
  combination_model(e1, e2, cmbn_fn = .Generic)
}

#' @export
Ops.mdl_ts <- function(e1, e2){
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
  
  # e_len <- c(length(e1), length(e2))
  # if(max(e_len) %% min(e_len) != 0){
  #   warn("longer object length is not a multiple of shorter object length")
  # }
  # 
  # if(e_len[[1]] != e_len[[2]]){
  #   if(which.min(e_len) == 1){
  #     is_mdl <- is_model(e1)
  #     cls <- class(e1)
  #     e1 <- rep_len(e1, e_len[[2]])
  #     if(is_mdl){
  #       e1 <- structure(e1, class = cls)
  #     }
  #   }
  #   else{
  #     is_mdl <- is_model(e2)
  #     cls <- class(e2)
  #     e2 <- rep_len(e2, e_len[[1]])
  #     if(is_mdl){
  #       e2 <- structure(e2, class = cls)
  #     }
  #   }
  # }
  
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
  list_of_models(map2(e1, e2, .Generic))
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
      cbind(
        residuals(object[[1]], type = "response")[[".resid"]],
        residuals(object[[2]], type = "response")[[".resid"]]
      ),
      na.rm = TRUE
    )
  }
  else{
    fc_cov <- 0
  }
  object[mdls] <- map(object[mdls], forecast, new_data = new_data, ...)
  object[mdls] <- map(object[mdls], function(x) x[[distribution_var(x)]])
  
  if(all(mdls)){
    fc_sd <- object %>% 
      map(function(x) sqrt(distributional::variance(x))) %>% 
      transpose_dbl()
    fc_cov <- suppressWarnings(stats::cov2cor(fc_cov))
    fc_cov[!is.finite(fc_cov)] <- 0 # In case of perfect forecasts
    fc_cov <- map_dbl(fc_sd, function(sigma) (diag(sigma)%*%fc_cov%*%t(diag(sigma)))[1,2])
  }
  
  is_normal <- map_lgl(object[mdls], function(x) inherits(x[[1]], "dist_normal"))
  if(all(is_normal)){ # Improve check to ensure all distributions are normal
    .dist <- eval_tidy(expr, object)
    # var(x) + var(y) + 2*cov(x,y)
    .dist <- distributional::dist_normal(mean(.dist), sqrt(distributional::variance(.dist) + 2*fc_cov))
  } else {
    .dist <- distributional::dist_degenerate(eval_tidy(expr, map(object, mean)))
  }
  
  .dist
}

#' @export
generate.model_combination <- function(x, new_data, specials, ...){
  if(".innov" %in% new_data){
    abort("Providing innovations for simulating combination models is not supported.")
  }
  
  mdls <- map_lgl(x, is_model)
  expr <- attr(x, "combination")
  x[mdls] <- map(x[mdls], generate, new_data, ...)
  out <- x[[which(mdls)[1]]]
  sims <- map(x, function(x) if(is_tsibble(x)) x[[".sim"]] else x)
  out[[".sim"]] <- eval_tidy(expr, sims)
  out
}

#' @export
fitted.model_combination <- function(object, ...){
  mdls <- map_lgl(object, is_model)
  expr <- attr(object, "combination")
  object[mdls] <- map(object[mdls], fitted, ...)
  fits <- map(object, function(x) if(is_tsibble(x)) x[[".fitted"]] else x)
  eval_tidy(expr, fits)
}
