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
#' @examplesIf requireNamespace("fable", quietly = TRUE)
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
#' @export
combination_model <- function(..., cmbn_fn = combination_ensemble,
                              cmbn_args = list()){
  mdls <- dots_list(...)
  if(!any(map_lgl(mdls, inherits, "mdl_defn"))){
    abort("`combination_model()` must contain at least one valid model definition.")
  }
  
  # Guess the response variable without transformations
  resp <- Reduce(intersect, lapply(Filter(function(x) inherits(x, "mdl_defn"), mdls), function(x) all.vars(model_lhs(x))))
  if(length(resp) == 0) abort("`combination_model()` must use component models with the same response variable.")
  
  cmbn_model <- new_model_class("cmbn_mdl", train = train_combination, 
                                specials = new_specials(xreg = function(...) NULL))
  fml <- mdls[[1]]$formula
  fml <- quo_set_expr(fml, sym(resp[[1]]))
  new_model_definition(cmbn_model, !!fml, ..., 
                       cmbn_fn = cmbn_fn, cmbn_args = cmbn_args)
}

#' Ensemble combination
#' 
#' @param ... Estimated models used in the ensemble.
#' @param weights The method used to weight each model in the ensemble.
#' 
#' @seealso [`combination_weighted()`]
#' 
#' @export
combination_ensemble <- function(..., weights = c("equal", "inv_var")){
  mdls <- dots_list(...)
  
  if(all(map_lgl(mdls, inherits, "mdl_defn"))){
    return(combination_model(..., cmbn_args = list(weights = weights)))
  }
  
  weights <- match.arg(weights)
  
  if(weights == "equal"){
    out <- do.call(mean, mdls)
  }
  else if(weights == "inv_var") {
    # For multi-series, each mdls[[i]] is an mdl_lst; index by series
    series_list <- if(is_model(mdls[[1]])) {
      list(mdls)
    } else {
      k <- length(mdls[[1]])
      map(seq_len(k), function(i) map(mdls, `[[`, i))
    }
    out <- map(series_list, function(series_models) {
      inv_var <- map_dbl(series_models, function(m) {
        1 / var(residuals(m, type = "response")[[".resid"]], na.rm = TRUE)
      })
      w <- inv_var / sum(inv_var)
      new_model_combination(series_models, w)
    })
    if(is_model(mdls[[1]])) out <- out[[1]]
  }
  
  if(is_model(out)){
    out$response <- mdls[[1]]$response
  }
  else{
    out <- structure(
      map(out, `[[<-`, "response", mdls[[1]][[1]]$response), 
      class = c("mdl_lst", "lst_mdl", "list"))
  }
  out
}

#' Weighted combination
#' 
#' @param ... Estimated models used in the ensemble.
#' @param weights The numeric weights applied to each model in `...`
#' 
#' @seealso [`combination_ensemble()`]
#' 
#' @export
combination_weighted <- function(..., weights = NULL){
  mdls <- dots_list(...)
  resp_var <- mdls[[1]]$response
  
  if(all(map_lgl(mdls, inherits, "mdl_defn"))){
    return(combination_model(..., cmbn_fn = combination_weighted,
                             cmbn_args = list(weights = weights)))
  }
  vctrs::vec_assert(weights, numeric(), length(mdls))
  
  # Normalisation is handled inside weighted.mean.mdl_ts / new_model_combination
  out <- do.call(weighted.mean, c(list(x = mdls[[1]], w = weights), mdls[-1]))
  out$response <- resp_var
  out
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

# Extract the component models and weights from any mdl_ts, treating a
# non-joint model as a single-element joint with weight 1.
.cj_parts <- function(m) {
  if(inherits(m[["fit"]], "model_combination")) {
    list(models = m[["fit"]]$models, weights = m[["fit"]]$weights)
  } else {
    list(models = list(m), weights = 1)
  }
}

#' @export
Ops.mdl_ts <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    warn(sprintf("`%s` not meaningful for models", .Generic))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic == "/" && !missing(e2) && is_model(e2)){
    warn("Cannot divide by a model")
    return(rep.int(NA, length(e1)))
  }
  if(.Generic == "*" && !missing(e2) && is_model(e1) && is_model(e2)){
    warn("Multiplying models is not supported")
    return(rep.int(NA, length(e1)))
  }
  
  # Unary + and -
  if(missing(e2)){
    if(.Generic == "+") return(e1)
    cmp <- .cj_parts(e1)
    return(new_model_combination(cmp$models, -cmp$weights))
  }
  
  # model +/- model: merge component lists, negating e2 weights for subtraction
  if(.Generic %in% c("+", "-") && is_model(e1) && is_model(e2)){
    c1   <- .cj_parts(e1)
    c2   <- .cj_parts(e2)
    sign <- if(.Generic == "+") 1 else -1
    return(new_model_combination(c(c1$models, c2$models), c(c1$weights, c2$weights * sign)))
  }
  
  # model * scalar  or  model / scalar: scale the weights
  if(.Generic %in% c("*", "/") && is_model(e1)){
    cmp    <- .cj_parts(e1)
    scalar <- if(.Generic == "*") e2 else 1/e2
    return(new_model_combination(cmp$models, cmp$weights * scalar))
  }
  # scalar * model
  if(.Generic == "*" && is_model(e2)){
    cmp <- .cj_parts(e2)
    return(new_model_combination(cmp$models, cmp$weights * e1))
  }
  
  warn(sprintf("`%s` not meaningful for models", .Generic))
  rep.int(NA, max(length(e1), length(e2)))
}

#' @export
Ops.mdl_lst <- function(e1, e2){
  list_of_models(map2(e1, e2, .Generic))
}
#' @export
Ops.lst_mdl <- deprecate_lst_mdl(Ops.mdl_lst)

# ---- Joint combination (N-way convolution solved in one step) ----

new_model_combination <- function(models, weights) {
  # Weighted sum of back-transformed training responses for data slot
  resp_data <- map(models, function(m) response(m)[[".response"]])
  resp <- Reduce(`+`, map2(resp_data, weights, `*`))
  
  comb_response <- models[[1]][["response"]]
  
  new_model(
    structure(
      list(models = models, weights = weights),
      class = "model_combination"
    ),
    model = models[[1]][["model"]],
    data = transmute(
      models[[1]][["data"]],
      !!expr_name(comb_response[[1]]) := resp
    ),
    response = comb_response,
    transformation = list(new_transformation(identity, identity))
  )
}

#' @export
model_sum.model_combination <- function(x) {
  "COMBINATION"
}

#' @importFrom stats var
#' @export
forecast.model_combination <- function(object, new_data, specials, ...) {
  models  <- object$models
  weights <- object$weights
  
  fcs   <- map(models, forecast, new_data = new_data, ...)
  dists <- map(fcs, function(x) x[[distribution_var(x)]])
  
  is_normal <- map_lgl(dists, function(x) all(dist_types(x) == "dist_normal"))
  
  if(all(is_normal)) {
    fc_means <- map(dists, mean)
    fc_vars  <- map(dists, distributional::variance)
    
    # Estimate correlation structure from in-sample residuals
    resids    <- map(models, function(m) residuals(m, type = "response")[[".resid"]])
    resid_mat <- do.call(cbind, resids)
    fc_cov    <- var(resid_mat, na.rm = TRUE)
    fc_cor    <- suppressWarnings(stats::cov2cor(fc_cov))
    fc_cor[!is.finite(fc_cor)] <- 0
    
    h <- length(dists[[1]])
    
    # mu_h  = sum_i  w_i * mu_i_h
    # var_h = sum_ij w_i * w_j * sigma_i_h * sigma_j_h * rho_ij
    #       = (w * sigma_h)^T Cor (w * sigma_h)   [avoids diag() scalar issue]
    mu <- map_dbl(seq_len(h), function(t) {
      sum(weights * map_dbl(fc_means, `[`, t))
    })
    sigma2 <- map_dbl(seq_len(h), function(t) {
      ws <- weights * sqrt(map_dbl(fc_vars, `[`, t))
      as.numeric(ws %*% fc_cor %*% ws)
    })
    
    distributional::dist_normal(mu, sqrt(pmax(sigma2, 0)))
  } else {
    pt_means <- map(dists, mean)
    h <- length(pt_means[[1]])
    mu <- map_dbl(seq_len(h), function(t) sum(weights * map_dbl(pt_means, `[`, t)))
    distributional::dist_degenerate(mu)
  }
}

#' @export
generate.model_combination <- function(x, new_data, specials, bootstrap = FALSE, ...) {
  if(".innov" %in% names(new_data)) {
    bootstrap <- TRUE
    new_data[[".innov"]] <- NULL
  }
  gens <- map(x$models, generate, new_data = new_data, bootstrap = bootstrap, ...)
  out  <- gens[[1]]
  sims <- map(gens, function(g) if(is_tsibble(g)) g[[".sim"]] else g)
  out[[".sim"]] <- Reduce(`+`, map2(sims, x$weights, `*`))
  out
}

#' @export
fitted.model_combination <- function(object, ...) {
  fits <- map(object$models, function(m) {
    f <- fitted(m, ...)
    if(is_tsibble(f)) f[[".fitted"]] else f
  })
  Reduce(`+`, map2(fits, object$weights, `*`))
}

#' @export
residuals.model_combination <- function(object, type = "response", ...) {
  res <- map(object$models, function(m) {
    r <- residuals(m, type = "response", ...)
    if(is_tsibble(r)) r[[".resid"]] else r
  })
  Reduce(`+`, map2(res, object$weights, `*`))
}

# ---- sum / mean / weighted.mean for model objects ----

#' Combine models jointly via sum, mean, or weighted mean
#' 
#' These methods allow multiple fitted models to be combined into a single
#' joint combination model. Unlike the pairwise arithmetic approach (e.g.
#' `(m1 + m2 + m3)/3`), these solve the N-way convolution of all component
#' distributions simultaneously using the full residual covariance matrix,
#' so the result is invariant to the order in which models are listed.
#' 
#' @param x The first model (or a list/`mdl_lst` of models).
#' @param ... Additional models to include in the combination.
#' @param w A numeric vector of weights, one per model (will be normalised to
#'   sum to 1).
#' @param na.rm Ignored; present for compatibility with the `Summary` generic.
#' 
#' @return A fitted combination model (`mdl_ts`) of class `model_combination`.
#' 
#' @seealso [combination_ensemble()], [combination_weighted()]
#' @keywords internal
#' @name model_combination_ops
NULL

#' @rdname model_combination_ops
#' @export
Summary.mdl_ts <- function(..., na.rm = FALSE) {
  switch(.Generic,
    sum = {
      mdls <- list(...)
      new_model_combination(mdls, rep(1, length(mdls)))
    },
    abort(paste0("`", .Generic, "` is not meaningful for model objects."))
  )
}

#' @rdname model_combination_ops
#' @export
mean.mdl_ts <- function(x, ...) {
  mdls <- c(list(x), list(...))
  n    <- length(mdls)
  new_model_combination(mdls, rep(1 / n, n))
}

#' @rdname model_combination_ops
#' @importFrom stats weighted.mean
#' @export
weighted.mean.mdl_ts <- function(x, w, ...) {
  mdls <- c(list(x), list(...))
  if(length(w) != length(mdls)) {
    abort(paste0(
      "`w` must have length ", length(mdls),
      " (one weight per model), but has length ", length(w), "."
    ))
  }
  new_model_combination(mdls, w / sum(w))
}

# mdl_lst variants apply the operation elementwise across series

#' @rdname model_combination_ops
#' @export
Summary.mdl_lst <- function(..., na.rm = FALSE) {
  switch(.Generic,
    sum = {
      mdls <- list(...)
      k    <- length(mdls[[1]])
      list_of_models(map(seq_len(k), function(i) {
        series_models <- map(mdls, `[[`, i)
        new_model_combination(series_models, rep(1, length(series_models)))
      }))
    },
    abort(paste0("`", .Generic, "` is not meaningful for model objects."))
  )
}

#' @rdname model_combination_ops
#' @export
mean.mdl_lst <- function(x, ...) {
  mdls <- c(list(x), list(...))
  n    <- length(mdls)
  k    <- length(mdls[[1]])
  list_of_models(map(seq_len(k), function(i) {
    series_models <- map(mdls, `[[`, i)
    new_model_combination(series_models, rep(1 / n, n))
  }))
}


#' @rdname model_combination_ops
#' @export
weighted.mean.mdl_lst <- function(x, w, ...) {
  mdls <- c(list(x), list(...))
  if(!is.numeric(w)) {
    cli::cli_abort(
      c("{.arg w} must be a numeric vector of weights.",
        "i" = "Did you forget to name the argument? Use {.code weighted.mean(..., w = <weights>)}.")
    )
  }
  if(length(w) != length(mdls)) {
    cli::cli_abort(
      c("{.arg w} must have length {length(mdls)} (one weight per {.cls mdl_lst}), but has length {length(w)}.")
    )
  }
  w <- w / sum(w)
  k <- length(mdls[[1]])
  list_of_models(map(seq_len(k), function(i) {
    series_models <- map(mdls, `[[`, i)
    new_model_combination(series_models, w)
  }))
}



