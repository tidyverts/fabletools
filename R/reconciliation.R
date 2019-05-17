#' Forecast reconciliation 
#' 
#' Specifies the method used to reconcile forecasts across keys.
#' 
#' @param data A mable.
#' @param ... Reconciliation specification
#' 
#' @export
reconcile <- function(data, ...){
  UseMethod("reconcile")
}

#' @export
reconcile.mdl_df <- function(data, ...){
  mutate(data, ...)
}

#' Minimum trace forecast reconciliation
#' 
#' @param mdls A column of models in a mable
#' @param method The reconciliation method to use.
#' 
#' @export
MinT <- function(mdls, method = c("shrink", "wls", "ols", "cov")){
  structure(mdls, class = c("lst_mint_mdl", "lst_mdl"),
            method = method)
}

#' @importFrom utils combn
#' @export
forecast.lst_mint_mdl <- function(object, key_data, ...){
  method <- object%@%"method"
  
  # Get forecasts
  fc <- NextMethod()
  fc_point <- fc %>% map(`[[`, expr_text(attr(fc[[1]],"response")[[1]])) %>% 
    invoke(cbind, .) %>% 
    as.matrix()
  fc_var <- fc %>% 
    map(`[[`, expr_text(attr(fc[[1]],"dist"))) %>% 
    map(function(x) map_dbl(x, `[[`, "sd")^2) %>% 
    transpose_dbl()
  
  # Compute weights (sample covariance)
  res <- map(object, function(x, ...) residuals(x, ...)[[2]], type = "response")
  res <- matrix(invoke(c, res), ncol = length(object))
  
  n <- nrow(res)
  covm <- crossprod(stats::na.omit(res)) / n
  if(method == "ols"){
    # OLS
    W <- diag(nrow = nrow(covm), ncol = ncol(covm))
  } else if(method == "wls"){
    # WLS
    W <- diag(diag(covm))
  } else if (method == "cov"){
    # MinT covariance
    W <- covm
  } else if (method == "shrink"){
    # MinT shrink
    tar <- diag(apply(res, 2, crossprod)/n)
    corm <- cov2cor(covm)
    xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
    v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
    diag(v) <- 0
    corapn <- cov2cor(tar)
    d <- (corm - corapn)^2
    lambda <- sum(v)/sum(d)
    lambda <- max(min(lambda, 1), 0)
    W <- lambda * tar + (1 - lambda) * covm
  } else {
    abort("Unknown reconciliation method")
  }
  
  
  # Check positive definiteness of weights
  eigenvalues <- eigen(W, only.values = TRUE)[["values"]]
  if (any(eigenvalues < 1e-8)) {
    abort("MinT needs covariance matrix to be positive definite.", call. = FALSE)
  }
  
  # Reconciliation matrices
  S <- build_smat(key_data)
  
  R1 <- stats::cov2cor(W)
  W_h <- map(fc_var, function(var) diag(sqrt(var))%*%R1%*%t(diag(sqrt(var))))
  
  S <- build_smat(key_data)
  R <- t(S)%*%solve(W)
  P <- solve(R%*%S)%*%R
  
  # Apply to forecasts
  fc_point <- S%*%P%*%t(fc_point)
  fc_point <- split(fc_point, row(fc_point))
  fc_var <- map(W_h, function(W) diag(S%*%P%*%W%*%t(P)%*%t(S)))
  fc_dist <- map2(fc_point, transpose_dbl(map(fc_var, sqrt)), dist_normal)
    
  # Update fables
  pmap(list(fc, fc_point, fc_dist), function(fc, point, dist){
    fc[[expr_text(attr(fc,"response")[[1]])]] <- point
    fc[[expr_text(attr(fc,"dist"))]] <- dist
    fc
  })
}

build_smat <- function(key_data){
  row_col <- sym(colnames(key_data)[length(key_data)])
  
  fct <- key_data %>%
    unnest(!!row_col) %>% 
    dplyr::arrange(!!row_col) %>% 
    select(!!expr(-!!row_col)) %>% 
    map(factor)
  
  smat <- map(fct, function(x){
    mat <- rep(0, length(x)*length(levels(x)))
    i <- which(!is.na(x))
    j <- as.numeric(x[i])
    mat[i + length(x) * (j-1)] <- 1
    mat <- matrix(mat, nrow = length(x), ncol = length(levels(x)))
    mat[is.na(x), ] <- 1
    mat
  })
  
  join_smat <- function(x, y){
    map(split(x, col(x)), `*`, y) %>% 
      invoke(cbind, .)
  }
  
  reduce(smat, join_smat)
}