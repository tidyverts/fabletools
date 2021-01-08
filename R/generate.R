#' Generate responses from a mable
#' 
#' Use a model's fitted distribution to simulate additional data with similar
#' behaviour to the response. This is a tidy implementation of 
#' `\link[stats]{simulate}`.
#' 
#' Innovations are sampled by the model's assumed error distribution. 
#' If `bootstrap` is `TRUE`, innovations will be sampled from the model's 
#' residuals. If `new_data` contains the `.innov` column, those values will be
#' treated as innovations for the simulated paths..
#' 
#' @param x A mable.
#' @param new_data The data to be generated (time index and exogenous regressors)
#' @param h The simulation horizon (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param times The number of replications.
#' @param seed The seed for the random generation from distributions.
#' @param ... Additional arguments for individual simulation methods.
#' 
#' @examples
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(dplyr)
#' UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)
#' UKLungDeaths %>% 
#'   model(lm = TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths)) %>% 
#'   generate(UKLungDeaths, times = 5)
#' }
#' @export
generate.mdl_df <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  mdls <- mable_vars(x)
  if(!is.null(new_data)){
    x <- bind_new_data(x, new_data)
  }
  kv <- c(key_vars(x), ".model")
  x <- tidyr::pivot_longer(as_tibble(x), all_of(mdls),
                           names_to = ".model", values_to = ".sim")
  
  # Evaluate simulations
  x[[".sim"]] <- map2(x[[".sim"]], 
                 x[["new_data"]] %||% rep(list(NULL), length.out = NROW(x)),
                 generate, h = h, times = times, seed = seed, ...)
  x[["new_data"]] <- NULL
  unnest_tsbl(x, ".sim", parent_key = kv)
}

#' @rdname generate.mdl_df
#' 
#' @param bootstrap If TRUE, then forecast distributions are computed using simulation with resampled errors.
#' @param bootstrap_block_size The bootstrap block size specifies the number of contiguous residuals to be taken in each bootstrap sample. 
#' 
#' @export
generate.mdl_ts <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, 
                            bootstrap = FALSE, bootstrap_block_size = 1, ...){
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  
  if(is.null(new_data)){
    new_data <- make_future_data(x$data, h)
  }
  
  if(is.null(new_data[[".rep"]])){
    kv <- c(".rep", key_vars(new_data))
    idx <- index_var(new_data)
    intvl <- tsibble::interval(new_data)
    new_data <- vctrs::vec_rbind(
      !!!set_names(rep(list(as_tibble(new_data)), times), seq_len(times)),
      .names_to = ".rep"
    )
    new_data <- build_tsibble(new_data, index = !!idx, key = !!kv, interval = intvl)
  }
  
  if(bootstrap) {
    res <- residuals(x$fit)
    res <- stats::na.omit(res) - mean(res, na.rm = TRUE)
    new_data$.innov <- if(bootstrap_block_size == 1) {
      sample(res, nrow(new_data), replace = TRUE)
    } else {
      if(any(has_gaps(x$data)$.gaps)) abort("Residuals must be regularly spaced without gaps to use a block bootstrap method.")
      kr <- tsibble::key_rows(new_data)
      # idx <- x$data[[index_var(x$data)]]
      # new_idx <- new_data[[index_var(new_data)]]
      # block_pos <- ((new_idx - min(idx))%%bootstrap_block_size)+1
      innov <- lapply(lengths(kr), function(n) block_bootstrap(res, bootstrap_block_size, size = n))
      vec_c(!!!innov)
    }
  }
  
  # Compute specials with new_data
  x$model$stage <- "generate"
  x$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(x$model),
                       error = function(e){
                         abort(sprintf(
                           "%s
Unable to compute required variables from provided `new_data`.
Does your model require extra variables to produce simulations?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  
  x$model$remove_data()
  x$model$stage <- NULL
  
  if(length(x$response) > 1) abort("Generating paths from multivariate models is not yet supported.")
  .sim <- generate(x[["fit"]], new_data = new_data, specials = specials, ...)
  .sim[[".sim"]] <- invert_transformation(x$transformation[[1]])(.sim[[".sim"]])
  .sim
}

block_bootstrap <- function (x, window_size, size = length(x)) {
  n_blocks <- size%/%window_size + 2
  bx <- numeric(n_blocks * window_size)
  for (i in seq_len(n_blocks)) {
    block_pos <- sample(seq_len(length(x) - window_size + 1), 1)
    bx[((i - 1) * window_size + 1):(i * window_size)] <- x[block_pos:(block_pos + window_size - 1)]
  }
  start_from <- sample(0:(window_size - 1), 1) + 1
  bx[seq(start_from, length.out = size)]
}