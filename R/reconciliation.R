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
  mutate(.data, ...)
}

#' Minimum trace forecast reconciliation
#' 
#' Reconciles a hierarchy using the minimum trace combination method. The 
#' response variable of the hierarchy must be aggregated using sums. The 
#' forecasted time points must match for all series in the hierarchy (caution:
#' this is not yet tested for beyond the series length).
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
    sparse <- requireNamespace("Matrix", quietly = TRUE)
  }
  structure(models, class = c("lst_mint_mdl", "lst_mdl", "list"),
            method = match.arg(method), sparse = sparse)
}

#' @export
forecast.lst_mint_mdl <- function(object, key_data, 
                                  new_data = NULL, h = NULL,
                                  point_forecast = list(.mean = mean), ...){
  method <- object%@%"method"
  sparse <- object%@%"sparse"
  if(sparse){
    require_package("Matrix")
    as.matrix <- Matrix::as.matrix
    t <- Matrix::t
    diag <- function(x) if(is.vector(x)) Matrix::Diagonal(x = x) else Matrix::diag(x)
    solve <- Matrix::solve
    cov2cor <- Matrix::cov2cor
  } else {
    cov2cor <- stats::cov2cor
  }
  
  point_method <- point_forecast
  point_forecast <- list()
  # Get forecasts
  fc <- NextMethod()
  if(length(unique(map(fc, interval))) > 1){
    abort("Reconciliation of temporal hierarchies is not yet supported.")
  }
  
  # Compute weights (sample covariance)
  res <- map(object, function(x, ...) residuals(x, ...), type = "response")
  if(length(unique(map_dbl(res, nrow))) > 1){
    # Join residuals by index #199
    res <- unname(as.matrix(reduce(res, full_join, by = index_var(res[[1]]))[,-1]))
  } else {
    res <- matrix(invoke(c, map(res, `[[`, 2)), ncol = length(object))
  }
  
  # Construct S matrix - ??GA: have moved this here as I need it for Structural scaling
  agg_data <- build_key_data_smat(key_data)
  
  n <- nrow(res)
  covm <- crossprod(stats::na.omit(res)) / n
  if(method == "ols"){
    # OLS
    W <- diag(rep(1L, nrow(covm)))
  } else if(method == "wls_var"){
    # WLS variance scaling
    W <- diag(diag(covm))
  } else if (method == "wls_struct"){
    # WLS structural scaling
    W <- diag(vapply(agg_data$agg,length,integer(1L)))
  } else if (method == "mint_cov"){
    # min_trace covariance
    W <- covm
  } else if (method == "mint_shrink"){
    # min_trace shrink
    tar <- diag(apply(res, 2, compose(crossprod, stats::na.omit))/n)
    corm <- cov2cor(covm)
    xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
    xs <- xs[stats::complete.cases(xs),]
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
    abort("min_trace needs covariance matrix to be positive definite.", call. = FALSE)
  }
  
  # Reconciliation matrices
  if(sparse){ 
    row_btm <- agg_data$leaf
    row_agg <- seq_len(nrow(key_data))[-row_btm]
    S <- Matrix::sparseMatrix(
      i = rep(seq_along(agg_data$agg), lengths(agg_data$agg)),
      j = vec_c(!!!agg_data$agg),
      x = rep(1, sum(lengths(agg_data$agg))))
    J <- Matrix::sparseMatrix(i = S[row_btm,,drop = FALSE]@i+1, j = row_btm, x = 1L, 
                              dims = rev(dim(S)))
    U <- cbind(
      Matrix::Diagonal(diff(dim(J))),
      -S[row_agg,,drop = FALSE]
    )
    U <- U[, order(c(row_agg, row_btm)), drop = FALSE]
    Ut <- t(U)
    WUt <- W %*% Ut
    P <- J - J %*% WUt %*% solve(U %*% WUt, U)
    # P <- J - J%*%W%*%t(U)%*%solve(U%*%W%*%t(U))%*%U
  }
  else {
    S <- matrix(0L, nrow = length(agg_data$agg), ncol = max(vec_c(!!!agg_data$agg)))
    S[length(agg_data$agg)*(vec_c(!!!agg_data$agg)-1) + rep(seq_along(agg_data$agg), lengths(agg_data$agg))] <- 1L
    R <- t(S)%*%solve(W)
    P <- solve(R%*%S)%*%R
  }
  
  reconcile_fbl_list(fc, S, P, W, point_forecast = point_method)
}

#' Bottom up forecast reconciliation
#' 
#' \lifecycle{experimental}
#' 
#' Reconciles a hierarchy using the bottom up reconciliation method. The 
#' response variable of the hierarchy must be aggregated using sums. The 
#' forecasted time points must match for all series in the hierarchy.
#' 
#' @param models A column of models in a mable.
#' 
#' @seealso 
#' [`reconcile()`], [`aggregate_key()`]
#' @export
bottom_up <- function(models){
  structure(models, class = c("lst_btmup_mdl", "lst_mdl", "list"))
}

#' @export
forecast.lst_btmup_mdl <- function(object, key_data, 
                                   point_forecast = list(.mean = mean), ...){
  # Keep only bottom layer
  agg_data <- build_key_data_smat(key_data)
  
  S <- matrix(0L, nrow = length(agg_data$agg), ncol = max(vec_c(!!!agg_data$agg)))
  S[length(agg_data$agg)*(vec_c(!!!agg_data$agg)-1) + rep(seq_along(agg_data$agg), lengths(agg_data$agg))] <- 1L
  
  btm <- agg_data$leaf
  object <- object[btm]
  
  point_method <- point_forecast
  point_forecast <- list()
  
  # Get base forecasts
  fc <- vector("list", nrow(S))
  fc[btm] <- NextMethod()
  
  # Add dummy forecasts to unused levels
  fc[seq_along(fc)[-btm]] <- fc[btm[1]]
  
  P <- matrix(0L, nrow = ncol(S), ncol = nrow(S))
  P[(btm-1L)*nrow(P) + seq_len(nrow(P))] <- 1L
  
  reconcile_fbl_list(fc, S, P, W = diag(nrow(S)),
                     point_forecast = point_method)
}


#' Top down forecast reconciliation
#' 
#' \lifecycle{experimental}
#' 
#' Reconciles a hierarchy using the top down reconciliation method. The 
#' response variable of the hierarchy must be aggregated using sums. The 
#' forecasted time points must match for all series in the hierarchy.
#' 
#' @param models A column of models in a mable.
#' @param method The reconciliation method to use.
#' 
#' @seealso 
#' [`reconcile()`], [`aggregate_key()`]
#' 
#' @export
top_down <- function(models, method = c("forecast_proportions", "average_proportions", "proportion_averages")){
  structure(models, class = c("lst_topdwn_mdl", "lst_mdl", "list"),
            method = match.arg(method))
}

#' @export
forecast.lst_topdwn_mdl <- function(object, key_data, 
                                    point_forecast = list(.mean = mean), ...){
  method <- object%@%"method"
  point_method <- point_forecast
  point_forecast <- list()
  
  agg_data <- build_key_data_smat(key_data)
  S <- matrix(0L, nrow = length(agg_data$agg), ncol = max(vec_c(!!!agg_data$agg)))
  S[length(agg_data$agg)*(vec_c(!!!agg_data$agg)-1) + rep(seq_along(agg_data$agg), lengths(agg_data$agg))] <- 1L
  
  # Identify top and bottom level
  top <- which.max(rowSums(S))
  btm <- which(rowSums(S) == 1L)
  
  kv <- names(key_data)[-ncol(key_data)]
  agg_shadow <- as_tibble(map(key_data[kv], is_aggregated))
  agg_struct <- vctrs::vec_unique(agg_shadow)
  agg_depth <- nrow(agg_struct)
  if(length(kv) != (agg_depth - 1)) {
    abort("Top down reconciliation requires strictly hierarchical structures.")
  }
  agg_order <- kv[order(vapply(agg_struct, sum, integer(1L)))]
  
  if(method == "forecast_proportions") {
    fc <- NextMethod()
    fc_dist <- lapply(fc, function(x) x[[distribution_var(x)]])
    fc_mean <- lapply(fc_dist, mean)
    fc_mean <- do.call(cbind, fc_mean)
    fc_prop <- matrix(1, nrow = nrow(fc_mean), ncol = ncol(fc_mean))
    
    # Ensure key structure matches order of fc object
    key_data <- key_data[order(vec_c(!!!key_data$.rows)),]
    for(i in seq_len(agg_depth - 1)) {
      agg_layer <- key_data[agg_order[seq_len(i)]]
      agg_nodes <- vec_group_loc(is_aggregated(agg_layer[[length(agg_layer)]]))
      # Drop nodes which aren't in this layer
      if((i+1) < agg_depth) {
        agg_keep <- which(is_aggregated(key_data[[agg_order[[i+1]]]]))
        agg_nodes$loc <- lapply(agg_nodes$loc, intersect, agg_keep)
      }
      # Identify index position of the layer's nodes and their parents
      agg_child_loc <- agg_nodes$loc[[which(!agg_nodes$key)]]
      agg_parent_loc <- agg_nodes$loc[[which(agg_nodes$key)]]
      agg_parent_key <- agg_layer[,-length(agg_layer)]
      agg_parent <- vec_match(agg_parent_key[agg_child_loc,,drop=FALSE], agg_parent_key[agg_parent_loc,,drop=FALSE])
      # Compute forecast proportions for this layer
      fc_prop[,agg_child_loc] <- fc_prop[,agg_parent_loc,drop=FALSE][,agg_parent,drop=FALSE] * fc_mean[,agg_child_loc,drop=FALSE] / t(rowsum(t(fc_mean[,agg_child_loc,drop=FALSE]), agg_parent))[,agg_parent]
    }
    # Code adapted from reconcile_fbl_list to handle changing weights over horizon
    # This will need to be refactored later so that reconcile_fbl_list is broken up into more sub-problems
    # As the weight matrix is an identity, this code and computation is much simpler.
    is_normal <- all(map_lgl(fc_dist, function(x) inherits(x[[1]], "dist_normal")))
    # Point forecast means can be computed in one step
    fc_mean <- split(fc_mean[,top]*fc_prop,col(fc_prop))
    if(is_normal) {
      fc_var <- map(fc_dist, distributional::variance)
      fc_var <- fc_prop * fc_var[[top]] * fc_prop
      fc_var <- split(fc_var, col(fc_var))
      fc_dist <- map2(fc_mean, map(fc_var, sqrt), distributional::dist_normal)
    } else {
      fc_dist <- lapply(fc_mean, distributional::dist_degenerate)
    }
    # Update fables
    fc <- map2(fc, fc_dist, function(fc, dist){
      dimnames(dist) <- dimnames(fc[[distribution_var(fc)]])
      fc[[distribution_var(fc)]] <- dist
      point_fc <- compute_point_forecasts(dist, point_method)
      fc[names(point_fc)] <- point_fc
      fc
    })
    return(fc)
    
  } else {
    # Compute dis-aggregation matrix
    history <- lapply(object, function(x) response(x)[[".response"]])
    top_y <- history[[top]]
    btm_y <- history[btm]
    if (method == "average_proportions") { 
      prop <- map_dbl(btm_y, function(y) mean(y/top_y))
    } else if (method == "proportion_averages") {
      prop <- map_dbl(btm_y, mean) / mean(top_y)
    } else {
      abort("Unkown `top_down()` reconciliation `method`.")
    }
    
    # Keep only top layer
    object <- object[top]
    
    # Get base forecasts
    fc <- vector("list", nrow(S))
    fc[top] <- NextMethod()
    
    # Add dummy forecasts to unused levels
    fc[seq_along(fc)[-top]] <- fc[top]
  }
  
  P <- matrix(0L, nrow = ncol(S), ncol = nrow(S))
  P[,top] <- prop
  
  reconcile_fbl_list(fc, S, P, W = diag(nrow(S)),
                     point_forecast = point_method)
}


#' Middle out forecast reconciliation
#' 
#' \lifecycle{experimental}
#' 
#' Reconciles a hierarchy using the middle out reconciliation method. The 
#' response variable of the hierarchy must be aggregated using sums. The 
#' forecasted time points must match for all series in the hierarchy.
#' 
#' @param models A column of models in a mable.
#' @param split The middle level of the hierarchy from which the bottom-up and
#' top-down approaches are used above and below respectively.
#' 
#' @seealso 
#' [`reconcile()`], [`aggregate_key()`]
#' [*Forecasting: Principles and Practice* - Middle-out approach](https://otexts.com/fpp3/single-level.html#middle-out-approach)
#' 
#' @export
middle_out <- function(models, split = 1){
  structure(models, class = c("lst_midout_mdl", "lst_mdl", "list"),
            split = split)
}

#' @export
forecast.lst_midout_mdl <- function(object, key_data, 
                                    point_forecast = list(.mean = mean), ...){
  split <- object%@%"split"
  point_method <- point_forecast
  point_forecast <- list()
  
  agg_data <- build_key_data_smat(key_data)
  S <- matrix(0L, nrow = length(agg_data$agg), ncol = max(vec_c(!!!agg_data$agg)))
  S[length(agg_data$agg)*(vec_c(!!!agg_data$agg)-1) + rep(seq_along(agg_data$agg), lengths(agg_data$agg))] <- 1L
  
  # Identify top and bottom level
  top <- which.max(rowSums(S))
  btm <- which(rowSums(S) == 1L)
  
  kv <- names(key_data)[-ncol(key_data)]
  agg_shadow <- as_tibble(map(key_data[kv], is_aggregated))
  agg_struct <- vctrs::vec_unique(agg_shadow)
  agg_depth <- nrow(agg_struct)
  if(length(kv) != (agg_depth - 1)) {
    abort("Top down reconciliation requires strictly hierarchical structures.")
  }
  agg_order <- kv[order(vapply(agg_struct, sum, integer(1L)))]
  if(is.character(split)) {
    split <- match(split, agg_order)
  }
  nodes_above <- which(agg_shadow[[split]])
  object <- object[-nodes_above]
  fc <- NextMethod()
  
  fc_dist <- lapply(fc, function(x) x[[distribution_var(x)]])
  h <- vec_size(fc_dist[[1]])
  fc_mean <- matrix(0, h, nrow(key_data))
  fc_mean[,-nodes_above] <- do.call(cbind, lapply(fc_dist, mean))
  
  fc_prop <- key_data[[split]]
  fc_prop <- matrix(1, nrow = nrow(fc_mean), ncol = vec_unique_count(fc_prop[!is_aggregated(fc_prop)]))
  
  # Ensure key structure matches order of fc object
  mid_root_nodes <- NULL
  key_data <- key_data[order(vec_c(!!!key_data$.rows)),]
  for(i in seq(split+1, length.out = agg_depth - 1 - split)) {
    agg_layer <- key_data[agg_order[seq_len(i)]][-nodes_above,]
    agg_nodes <- vec_group_loc(is_aggregated(agg_layer[[length(agg_layer)]]))
    # Drop nodes which aren't in this layer
    if((i+1) < agg_depth) {
      agg_keep <- which(is_aggregated(key_data[[agg_order[[i+1]]]]))
      agg_nodes$loc <- lapply(agg_nodes$loc, intersect, agg_keep)
    }
    # Identify index position of the layer's nodes and their parents
    agg_child_loc <- agg_nodes$loc[[which(!agg_nodes$key)]]
    agg_parent_loc <- agg_nodes$loc[[which(agg_nodes$key)]]
    agg_parent_key <- agg_layer[,-length(agg_layer)]
    agg_parent <- vec_match(agg_parent_key[agg_child_loc,,drop=FALSE], agg_parent_key[agg_parent_loc,,drop=FALSE])
    
    # Produce matching replications of middle layer node positions
    if(is.null(mid_root_nodes)) mid_root_nodes <- agg_parent_loc
    mid_root_nodes <- mid_root_nodes[agg_parent]
    # Compute forecast proportions for this layer
    fc_prop <- fc_prop[,agg_parent,drop=FALSE] * fc_mean[,agg_child_loc,drop=FALSE] / t(rowsum(t(fc_mean[,agg_child_loc,drop=FALSE]), agg_parent))[,agg_parent]
  }
  
  fc_mean <- (fc_prop*fc_mean[,mid_root_nodes])%*%t(S)
  # Code adapted from reconcile_fbl_list to handle changing weights over horizon
  # This will need to be refactored later so that reconcile_fbl_list is broken up into more sub-problems
  # As the weight matrix is an identity, this code and computation is much simpler.
  is_normal <- all(map_lgl(fc_dist, function(x) inherits(x[[1]], "dist_normal")))
  # Point forecast means can be computed in one step
  fc_mean <- split(fc_mean,col(fc_mean))
  if(is_normal) {
    fc_var <- vector("list", nrow(key_data))
    fc_var[-nodes_above] <- map(fc_dist, distributional::variance)
    fc_var[nodes_above] <- rep_len(list(double(h)), length(nodes_above))
    
    P <- matrix(0L, nrow = ncol(S), ncol = nrow(S))
    
    # (S%*%P)%*%t(fc_mean)
    fc_var <- map(seq_len(h), function(i) {
      # Add top down structure
      P[seq_along(mid_root_nodes) + (mid_root_nodes-1)*nrow(P)] <- fc_prop[i,]
      SP <- S%*%P
      diag(SP%*%diag(map_dbl(fc_var, `[[`, i))%*%t(SP))
    })
    fc_dist <- map2(fc_mean, transpose_dbl(map(fc_var, sqrt)), distributional::dist_normal)
    
  } else {
    fc_dist <- lapply(fc_mean, distributional::dist_degenerate)
  }
  
  # Update fables
  map2(rep(fc[1], nrow(key_data)), fc_dist, function(fc, dist){
    dimnames(dist) <- dimnames(fc[[distribution_var(fc)]])
    fc[[distribution_var(fc)]] <- dist
    point_fc <- compute_point_forecasts(dist, point_method)
    fc[names(point_fc)] <- point_fc
    fc
  })
}

reconcile_fbl_list <- function(fc, S, P, W, point_forecast, SP = NULL) {
  if(length(unique(map(fc, interval))) > 1){
    abort("Reconciliation of temporal hierarchies is not yet supported.")
  }
  if(!inherits(S, "matrix")) {
    # Use sparse functions
    require_package("Matrix")
    as.matrix <- Matrix::as.matrix
    t <- Matrix::t
    diag <- function(x) if(is.vector(x)) Matrix::Diagonal(x = x) else Matrix::diag(x)
    cov2cor <- Matrix::cov2cor
  } else {
    cov2cor <- stats::cov2cor
  }
  if(is.null(SP)) {
    SP <- S%*%P
  }
  
  fc_dist <- map(fc, function(x) x[[distribution_var(x)]])
  is_normal <- all(map_lgl(fc_dist, function(x) inherits(x[[1]], "dist_normal")))
  
  fc_mean <- as.matrix(invoke(cbind, map(fc_dist, mean)))
  fc_var <- transpose_dbl(map(fc_dist, distributional::variance))
  
  # Apply to forecasts
  fc_mean <- as.matrix(SP%*%t(fc_mean))
  fc_mean <- split(fc_mean, row(fc_mean))
  if(is_normal){
    R1 <- cov2cor(W)
    W_h <- map(fc_var, function(var) diag(sqrt(var))%*%R1%*%t(diag(sqrt(var))))
    fc_var <- map(W_h, function(W) diag(SP%*%W%*%t(SP)))
    fc_dist <- map2(fc_mean, transpose_dbl(map(fc_var, sqrt)), distributional::dist_normal)
  } else {
    fc_dist <- map(fc_mean, distributional::dist_degenerate)
  }
  
  # Update fables
  map2(fc, fc_dist, function(fc, dist){
    dimnames(dist) <- dimnames(fc[[distribution_var(fc)]])
    fc[[distribution_var(fc)]] <- dist
    point_fc <- compute_point_forecasts(dist, point_forecast)
    fc[names(point_fc)] <- point_fc
    fc
  })
}

build_smat_rows <- function(key_data){
  lifecycle::deprecate_warn("0.2.1", "fabletools::build_smat_rows()", "fabletools::build_key_data_smat()")
  row_col <- sym(colnames(key_data)[length(key_data)])
  
  smat <- key_data %>%
    unnest(!!row_col) %>% 
    dplyr::arrange(!!row_col) %>% 
    select(!!expr(-!!row_col))
  
  agg_struc <- group_data(dplyr::group_by_all(as_tibble(map(smat, is_aggregated))))
  
  # key_unique <- map(smat, function(x){
  #   x <- unique(x)
  #   x[!is_aggregated(x)]
  # })
  
  agg_struc$.smat <- map(agg_struc$.rows, function(n) diag(1, nrow = length(n), ncol = length(n)))
  agg_struc <- map(seq_len(nrow(agg_struc)), function(i) agg_struc[i,])
  
  out <- reduce(agg_struc, function(x, y){
    # For now, assume x is aggregated into y somehow
    n_key <- ncol(x)-2
    nm_key <- names(x)[seq_len(n_key)]
    agg_vars <- map2_lgl(x[seq_len(n_key)], y[seq_len(n_key)], `<`)
    
    if(!any(agg_vars)) abort("Something unexpected happened, please report this bug at https://github.com/tidyverts/fabletools/issues/ with a description of what you're trying to do.")
    
    # Match rows between summation matrices
    not_agg <- names(Filter(`!`, y[seq_len(n_key)]))
    cols <- group_data(group_by(smat[x$.rows[[1]][seq_len(ncol(x$.smat[[1]]))],], !!!syms(not_agg)))$.rows
    cols_pos <- unlist(cols)
    cols <- rep(seq_along(cols), map_dbl(cols, length))
    cols[cols_pos] <- cols
    
    x$.rows[[1]] <- c(x$.rows[[1]], y$.rows[[1]])
    x$.smat <- list(rbind(
      x$.smat[[1]],
      y$.smat[[1]][, cols, drop = FALSE]
    ))
    x
  })
  
  smat <- out$.smat[[1]]
  smat[out$.rows[[1]],] <- smat
  
  return(smat)
}

build_key_data_smat <- function(x){
  kv <- names(x)[-ncol(x)]
  agg_shadow <- as_tibble(map(x[kv], is_aggregated))
  grp <- as_tibble(vctrs::vec_group_loc(agg_shadow))
  num_agg <- rowSums(grp$key)
  # Initialise comparison leafs with known/guaranteed leafs
  x_leaf <- x[vec_c(!!!grp$loc[which(num_agg == min(num_agg))]),]
  
  # Sort by disaggregation to identify aggregated leafs in order
  grp <- grp[order(num_agg),]
  
  grp$match <- lapply(unname(split(grp, seq_len(nrow(grp)))), function(level){
    disagg_col <- which(!vec_c(!!!level$key))
    agg_idx <- level[["loc"]][[1]]
    pos <- vec_match(x_leaf[disagg_col], x[agg_idx, disagg_col])
    pos <- vec_group_loc(pos)
    pos <- pos[!is.na(pos$key),]
    # Add non-matches as leaf nodes
    agg_leaf <- setdiff(seq_along(agg_idx), pos$key)
    if(!is_empty(agg_leaf)){
      pos <- vec_rbind(
        pos,
        structure(list(key = agg_leaf, loc = as.list(seq_along(agg_leaf) + nrow(x_leaf))), 
                  class = "data.frame", row.names = agg_leaf)
      )
      x_leaf <<- vec_rbind(
        x_leaf, 
        x[agg_idx[agg_leaf],]
      )
    }
    pos$loc[order(pos$key)]
  })
  if(any(lengths(grp$loc) != lengths(grp$match))) {
    abort("An error has occurred when constructing the summation matrix.\nPlease report this bug here: https://github.com/tidyverts/fabletools/issues")
  }
  idx_leaf <- vec_c(!!!x_leaf$.rows)
  x$.rows[unlist(x$.rows)[vec_c(!!!grp$loc)]] <- vec_c(!!!grp$match)
  return(list(agg = x$.rows, leaf = idx_leaf))
  # out <- matrix(0L, nrow = nrow(x), ncol = length(idx_leaf))
  # out[nrow(x)*(vec_c(!!!x$.rows)-1) + rep(seq_along(x$.rows), lengths(x$.rows))] <- 1L
  # out
}
