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
  message("Note: reconciliation in fable is highly experimental. The interface will be refined in the near future.")
  mutate(data, ...)
}

#' Minimum trace forecast reconciliation
#' 
#' @param mdls A column of models in a mable.
#' @param method The reconciliation method to use.
#' @param sparse Should the reconciliation be computed with sparse matrix algebra?
#' 
#' @export
MinT <- function(mdls, method = c("shrink", "wls", "ols", "cov"), sparse = FALSE){
  structure(mdls, class = c("lst_mint_mdl", "lst_mdl"),
            method = match.arg(method), sparse = sparse)
}

#' @importFrom utils combn
#' @export
forecast.lst_mint_mdl <- function(object, key_data, ...){
  method <- object%@%"method"
  sparse <- object%@%"sparse"
  
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
    corm <- stats::cov2cor(covm)
    xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
    v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
    diag(v) <- 0
    corapn <- stats::cov2cor(tar)
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
  
  if(sparse){
    require_package("Matrix")
    require_package("SparseM")
    require_package("methods")
    as.matrix <- SparseM::as.matrix
    t <- SparseM::t
    diag <- SparseM::diag
    
    S <- SparseM::as.matrix.csr(S)
    rs <- as.matrix(S%*%matrix(1, nrow=32))
    row_agg <- which(rs!=1)
    row_btm <- which(rs==1)
    i_pos <- which(as.logical(as.matrix(S[row_btm,])))
    J <- methods::new("matrix.csr", ra = rep(1,ncol(S)), ja =row_btm,
                      ia = c((i_pos-1L)%/%ncol(S)+1L, ncol(S) + 1L), dimension = rev(dim(S)))
    
    U <- cbind(methods::as(diff(dim(J)), "matrix.diag.csr"), SparseM::as.matrix.csr(-S[row_agg,]))
    U <- U[, order(c(row_agg, row_btm))]
    
    P <- J - J%*%W%*%t(U)%*%Matrix::solve(U%*%W%*%t(U))%*%U
  }
  else {
    R <- t(S)%*%solve(W)
    P <- solve(R%*%S)%*%R
  }
  
  
  # Apply to forecasts
  fc_point <- as.matrix(S%*%P%*%t(fc_point))
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