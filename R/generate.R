#' Generate responses from a mable
#' 
#' Use a model's fitted distribution to simulate additional data with similar
#' behaviour to the response. This is a tidy implementation of 
#' [stats::simulate()].
#' 
#' Innovations are sampled by the model's assumed error distribution. 
#' If `bootstrap` is `TRUE`, innovations will be sampled from the model's 
#' residuals. If `new_data` contains the `.innov` column, those values will be
#' treated as innovations for the simulated paths.
#' 
#' @param x A mable.
#' @param new_data The data to be generated (time index and exogenous regressors)
#' @param h The simulation horizon (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param times The number of replications.
#' @param seed `r lifecycle::badge('deprecated')` Call [set.seed()] before `generate()` instead.
#' @param ... Additional arguments for individual simulation methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(dplyr)
#' UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)
#' UKLungDeaths %>% 
#'   model(lm = TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths)) %>% 
#'   generate(UKLungDeaths, times = 5)
#' @export
generate.mbl_df <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  mdls <- mable_vars(x)
  # A `.rep` column is allowed in `new_data`, where it identifies replications.
  check_reserved_names(x, new_data, key = c(".model", ".rep"), data = ".model")
  if(!is.null(new_data)){
    x <- bind_new_data(x, new_data)
  }
  kv <- c(key_vars(x), ".model")
  x <- as_tibble(x)
  
  # Model groups (`mdl_df`) are simulated as a whole, keeping any joint
  # behaviour they have, before the remaining models are simulated separately.
  grps <- mdls[map_lgl(x[mdls], is_mdl_df)]
  sims <- unpack_model_results(
    map(x[grps], generate, new_data = x[["new_data"]],
        h = h, times = times, seed = seed, ...)
  )
  mdls <- setdiff(mdls, grps)
  if(!is_empty(grps)) {
    # Allow the remaining models to be pivoted alongside the simulations
    x[mdls] <- map(x[mdls], vec_data)
  }
  x <- vec_cbind(x[setdiff(names(x), grps)], tibble::new_tibble(sims, nrow = NROW(x)))
  x <- tidyr::pivot_longer(x, all_of(c(mdls, names(sims))),
                           names_to = ".model", values_to = ".sim")
  
  # Evaluate simulations
  x[[".sim"]] <- map2(x[[".sim"]], 
                 x[["new_data"]] %||% rep(list(NULL), length.out = NROW(x)),
                 function(mdl, new_data) {
                   if(!is_model(mdl)) return(mdl)
                   generate(mdl, new_data, h = h, times = times, seed = seed, ...)
                 })
  x[["new_data"]] <- NULL
  unnest_tsbl(x, ".sim", parent_key = kv)
}

#' @export
generate.mdl_lst <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...) {
  map2(x, 
       new_data %||% rep(list(NULL), length.out = NROW(x)),
       generate, h = h, times = times, seed = seed, ...)
}
#' @export
generate.lst_mdl <- deprecate_lst_mdl(generate.mdl_lst)

#' @rdname generate.mbl_df
#'
#' @param bootstrap `r lifecycle::badge('deprecated')` Please use [bootstrap_iid()] or [bootstrap_block()] to wrap the model instead.
#' @param bootstrap_block_size `r lifecycle::badge('deprecated')` Please use [bootstrap_block()]'s `block_size` argument instead.
#'
#' @export
generate.mdl_ts <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL,
                            bootstrap = FALSE, bootstrap_block_size = 1, ...){
  if(isTRUE(bootstrap)) {
    lifecycle::deprecate_warn(
      "1.0.0", "generate(bootstrap = )", "bootstrap_iid()",
      details = "Wrap the model with `bootstrap_iid()` or `bootstrap_block()` (via `mutate()`) instead of passing `bootstrap = TRUE` to `generate()`."
    )
    x <- if(bootstrap_block_size > 1) {
      bootstrap_block(x, block_size = bootstrap_block_size)
    } else {
      bootstrap_iid(x)
    }
    return(generate(x, new_data = new_data, h = h, times = times, seed = seed, ...))
  }

  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))

  generate_mdl_ts_assemble(x, setup$new_data, ...)
}

#' @rdname generate.mbl_df
#' @export
generate.mdl_bootstrap_iid <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))
  new_data <- setup$new_data

  # Respect an already-populated .innov (set by joint
  # bootstrap_joint_new_data()) rather than resampling.
  if (is.null(new_data[[".innov"]])) {
    res <- bootstrap_residual_index(x)$resid
    i <- sample.int(NROW(res), nrow(new_data), replace = TRUE)
    new_data$.innov <- if(length(x$response) == 1) res[i] else res[i,]
  }

  generate_mdl_ts_assemble(x, new_data, ...)
}

#' @rdname generate.mbl_df
#' @export
generate.mdl_bootstrap_block <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))
  new_data <- setup$new_data

  if (is.null(new_data[[".innov"]])) {
    if(any(has_gaps(x$data)$.gaps)) abort("Residuals must be regularly spaced without gaps to use a block bootstrap method.")
    res <- bootstrap_residual_index(x)$resid
    block_size <- x%@%"block_size"
    kr <- tsibble::key_rows(new_data)
    ki <- lapply(lengths(kr), function(n) block_bootstrap(NROW(res), block_size, size = n))
    i <- vec_c(!!!ki)
    new_data$.innov <- if(length(x$response) == 1) res[i] else res[i,]
  }

  generate_mdl_ts_assemble(x, new_data, ...)
}

# Shared setup for generate.mdl_ts()/generate.mdl_bootstrap_iid()/
# generate.mdl_bootstrap_block(): establishes the RNG state (the caller is
# responsible for restoring the returned RNGstate via on.exit() once it has
# `seed`, since on.exit() must be registered in the caller's own frame),
# resolves/replicates new_data, and reseeds lag()'s short term memory.
generate_mdl_ts_setup <- function(x, new_data, h, times, seed) {
  RNGstate <- NULL
  if (!is.null(seed)) {
    lifecycle::deprecate_warn(
      "1.0.0", "generate(seed = )",
      details = "Call `set.seed()` before `generate()` instead of passing `seed=`."
    )
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      stats::runif(1)
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
  }

  if(is.null(new_data)){
    new_data <- make_future_data(x$data, h)
  }

  # Reseed lag()'s short term memory from this fit's own snapshot.
  x$model$recent_data <- attr(x, "recent_data")

  new_data <- replicate_new_data(new_data, times)

  list(new_data = new_data, RNGstate = RNGstate)
}

# Shared assembly for generate.mdl_ts()/generate.mdl_bootstrap_iid()/
# generate.mdl_bootstrap_block(): computes specials, simulates from the
# fitted model (using new_data$.innov if a modifier has populated it), and
# back-transforms the result. Kept as a single shared step so it's never
# duplicated across model modifiers.
generate_mdl_ts_assemble <- function(x, new_data, ...) {
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

  .sim <- generate(x[["fit"]], new_data = new_data, specials = specials, ...)
  .sim_cols <- setdiff(names(.sim), names(new_data))

  # TODO: Breaking change, .sim should be response variable name
  # For now, only do this with multivariate models but this change will be made for v1.0.0
  resp_vars <- vapply(x$response, expr_name, character(1L), USE.NAMES = FALSE)
  if (length(resp_vars) > 1) {
    .sim_cols <- resp_vars
    .sim[resp_vars] <- split(.sim$.sim, col(.sim$.sim))
    .sim$.sim <- NULL
  }

  # Back-transform forecast distributions
  bt <- map(x$transformation, function(x){
    bt <- invert_transformation(x)
    env <- new_environment(new_data, get_env(bt))
    set_env(bt, env)
  })

  .sim[.sim_cols] <- .mapply(function(f, x) f(x), list(bt, .sim[.sim_cols]), NULL)
  .sim
}

# Demeaned innovation residuals paired with their calendar time index.
bootstrap_residual_index <- function(x) {
  res <- residuals(x$fit, type = "innovation")
  idx <- x$data[[index_var(x$data)]]
  keep <- if (is.matrix(res)) stats::complete.cases(res) else !is.na(res)
  res <- if (is.matrix(res)) res[keep, , drop = FALSE] else res[keep]
  idx <- idx[keep]
  f_mean <- if(length(x$response) == 1) mean else colMeans
  list(index = idx, resid = res - f_mean(res))
}

# Replicate new_data into times copies stacked with a .rep key column;
# no-op if already replicated.
replicate_new_data <- function(new_data, times) {
  if (!is.null(new_data[[".rep"]])) return(new_data)
  kv <- c(".rep", key_vars(new_data))
  idx <- index_var(new_data)
  intvl <- tsibble::interval(new_data)
  new_data <- vctrs::vec_rbind(
    !!!set_names(rep(list(as_tibble(new_data)), times), seq_len(times)),
    .names_to = ".rep"
  )
  build_tsibble(new_data, index = !!idx, key = !!kv, interval = intvl)
}

block_bootstrap <- function (n, window_size, size = n) {
  n_blocks <- size%/%window_size + 2
  bx <- numeric(n_blocks * window_size)
  for (i in seq_len(n_blocks)) {
    block_pos <- sample(seq_len(n - window_size + 1), 1)
    bx[((i - 1) * window_size + 1):(i * window_size)] <- block_pos:(block_pos + window_size - 1)
  }
  start_from <- sample.int(window_size, 1)
  bx[seq(start_from, length.out = size)]
}