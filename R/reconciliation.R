#' Forecast reconciliation 
#' 
#' This function allows you to specify the method used to reconcile forecasts
#' in accordance with its key structure.
#' 
#' @param .data A mable.
#' @param ... Reconciliation methods applied to model columns within `.data`.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' lung_deaths_agg <- as_tsibble(cbind(mdeaths, fdeaths)) %>%
#'   aggregate_key(key, value = sum(value))
#' 
#' lung_deaths_agg %>%
#'   model(lm = TSLM(value ~ trend() + season())) %>%
#'   reconcile(lm = min_trace(lm)) %>% 
#'   forecast()
#' }
#' 
#' @export
reconcile <- function(.data, ...){
  UseMethod("reconcile")
}

#' @rdname reconcile
#' @export
reconcile.mdl_df <- function(.data, ...){
  warn("Reconciliation in fable is highly experimental. The interface will likely be refined in the near future.")
  mutate(.data, ...)
}

#' Minimum trace forecast reconciliation
#' 
#' Reconciles a hierarchy using the minimum trace combination method. The 
#' response variable of the hierarchy must be aggregated using sums.
#' 
#' @param models A column of models in a mable.
#' @param method The reconciliation method to use.
#' @param sparse If TRUE, the reconciliation will be computed using sparse 
#' matrix algebra? By default, sparse matrices will be used if the MatrixM 
#' package is installed.
#' 
#' @seealso 
#' [`reconcile()`], [`aggregate_key()`]
#' 
#' @references 
#' Wickramasuriya, S. L., Athanasopoulos, G., & Hyndman, R. J. (2019). Optimal forecast reconciliation for hierarchical and grouped time series through trace minimization. Journal of the American Statistical Association, 1-45. https://doi.org/10.1080/01621459.2018.1448825 
#' 
#' @export
min_trace <- function(models, method = c("wls_var", "ols", "wls_struct", "mint_cov", "mint_shrink"),
                 sparse = NULL){
  if(is.null(sparse)){
    sparse <- requireNamespace("SparseM", quietly = TRUE)
  }
  structure(models, class = c("lst_mint_mdl", "lst_mdl"),
            method = match.arg(method), sparse = sparse)
}

#' @importFrom utils combn
#' @export
forecast.lst_mint_mdl <- function(object, key_data, ...){
  method <- object%@%"method"
  sparse <- object%@%"sparse"
  
  # Get forecasts
  fc <- NextMethod()
  if(length(unique(map(fc, interval))) > 1){
    abort("Reconciliation of temporal hierarchies is not yet supported.")
  }
  fc_point <- fc %>% 
    map(`[[`, expr_text(attr(fc[[1]],"response")[[1]])) %>% 
    invoke(cbind, .) %>% 
    as.matrix()
  fc_var <- fc %>% 
    map(`[[`, expr_text(attr(fc[[1]],"dist"))) %>% 
    map(function(x){
      if(!is_dist_normal(x)) abort("Reconciliation of non-normal forecasts is not yet supported.")
      map_dbl(x, `[[`, "sd")^2
    }) %>% 
    transpose_dbl()
  
  # Coonstruct mpute S martrix - ??GA: have moved this here as I need it for Structural scaling
  S <- build_smat(key_data)

  # Compute weights (sample covariance)
  res <- map(object, function(x, ...) residuals(x, ...)[[2]], type = "response")
  res <- matrix(invoke(c, res), ncol = length(object))
  
  n <- nrow(res)
  covm <- crossprod(stats::na.omit(res)) / n
  if(method == "ols"){
    # OLS
    W <- diag(nrow = nrow(covm), ncol = ncol(covm))
  } else if(method == "wls_var"){
    # WLS variance scaling
    W <- diag(diag(covm))
  } else if (method == "wls_struct"){
    # WLS structural scaling
    W <- diag(apply(S,1,sum))
  } else if (method == "mint_cov"){
    # min_trace covariance
    W <- covm
  } else if (method == "mint_shrink"){
    # min_trace shrink
    tar <- diag(apply(res, 2, compose(crossprod, stats::na.omit))/n)
    corm <- stats::cov2cor(covm)
    xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
    xs <- xs[stats::complete.cases(xs),]
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
    abort("min_trace needs covariance matrix to be positive definite.", call. = FALSE)
  }
  
  # Reconciliation matrices
  R1 <- stats::cov2cor(W)
  W_h <- map(fc_var, function(var) diag(sqrt(var))%*%R1%*%t(diag(sqrt(var))))
  
  if(sparse){
    require_package("SparseM")
    require_package("methods")
    as.matrix <- SparseM::as.matrix
    t <- SparseM::t
    diag <- SparseM::diag
    
    row_btm <- key_data %>%
      dplyr::filter(
        !!!map(colnames(key_data[-length(key_data)]), function(x){
          expr(!is_aggregated(!!sym(x)))
        })
      )
    row_btm <- as.integer(row_btm[[length(row_btm)]])
    row_agg <- seq_len(NROW(key_data))[-row_btm]
    
    i_pos <- which(as.logical(S[row_btm,]))
    S <- SparseM::as.matrix.csr(S)
    J <- methods::new("matrix.csr", ra = rep(1,ncol(S)), ja = row_btm,
                      ia = c((i_pos-1L)%/%ncol(S)+1L, ncol(S) + 1L), dimension = rev(dim(S)))
    
    U <- cbind(methods::as(diff(dim(J)), "matrix.diag.csr"), SparseM::as.matrix.csr(-S[row_agg,]))
    U <- U[, order(c(row_agg, row_btm))]
    
    P <- J - J%*%W%*%t(U)%*%SparseM::solve(U%*%W%*%t(U), eps = Inf)%*%U
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
    dplyr::mutate_all(factor)
  
  lvls <- invoke(paste, fct[stats::complete.cases(fct),])
  
  smat <- map(fct, function(x){
    mat <- rep(0, length(x)*length(levels(x)))
    i <- which(!is.na(x))
    if(length(i) == length(x) && length(levels(x)) > 1){
      abort("Reconciliation of disjoint hierarchical structures is not yet supported.")
    }
    j <- as.numeric(x[i])
    mat[i + length(x) * (j-1)] <- 1
    mat <- matrix(mat, nrow = length(x), ncol = length(levels(x)),
                  dimnames = list(NULL, levels(x)))
    mat[is.na(x), ] <- 1
    mat
  })
  
  join_smat <- function(x, y){
    map(split(x, col(x)), `*`, y) %>% 
      map2(colnames(x), function(S, cn) `colnames<-`(S, paste(cn, colnames(S)))) %>% 
      invoke(cbind, .)
  }
  
  reduce(smat, join_smat)[,lvls,drop = FALSE]
}