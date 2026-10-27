#' Generate responses from a mable
#' 
#' Use a model's fitted distribution to simulate additional data with similar
#' behaviour to the response. This is a tidy implementation of 
#' [stats::simulate()].
#' 
#' Innovations are sampled by the model's assumed error distribution, or
#' resampled from the model's residuals for models modified with
#' [bootstrap_iid()] or [bootstrap_block()]. If `new_data` contains the
#' `.innov` column, those values will be treated as innovations for the
#' simulated paths.
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
  h <- check_horizon(new_data, h)
  # A `.rep` column is allowed in `new_data`, where it identifies replications.
  check_reserved_names(x, new_data, key = c(".model", ".rep"), data = ".model")
  if (!is.null(seed)) {
    lifecycle::deprecate_warn(
      "1.0.0", "generate(seed = )",
      details = "Call `set.seed()` before `generate()` instead of passing `seed`.",
      user_env = caller_env()
    )
  }
  dots <- list2(...)
  x <- modify_each_model(x, deprecated_sim_modifier(dots, "generate", user_env = caller_env()))
  dots[c("bootstrap", "bootstrap_block_size")] <- NULL
  inject(dispatch_mbl_df(
    x, h = h, times = times, seed = seed, !!!dots,
    .f = generate, .new_data = new_data, .values_to = ".sim", .unnest = "tsibble"
  ))
}

#' @export
generate.mdl_df <- mdl_df_method(generate)

#' @export
generate.mdl_lst <- mdl_lst_method(generate, new_data = TRUE)
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
    modifier <- deprecated_sim_modifier(
      list(bootstrap = bootstrap, bootstrap_block_size = bootstrap_block_size),
      "generate", user_env = caller_env()
    )
    return(generate(modifier(x), new_data = new_data, h = h, times = times, seed = seed, ...))
  }

  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))

  generate_mdl_ts_assemble(x, setup$new_data, ...)
}

#' @export
generate.mdl_bootstrap_iid <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))
  x <- bootstrap_combination(x, setup$new_data)
  new_data <- bootstrap_innovations(x, setup$new_data)
  generate_mdl_ts_assemble(x, new_data, ...)
}

#' @export
generate.mdl_bootstrap_block <- function(x, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  setup <- generate_mdl_ts_setup(x, new_data, h, times, seed)
  if (!is.null(seed)) on.exit(assign(".Random.seed", setup$RNGstate, envir = .GlobalEnv))
  x <- bootstrap_combination(x, setup$new_data, block_size = x%@%"block_size")
  new_data <- bootstrap_innovations(x, setup$new_data, block_size = x%@%"block_size")
  generate_mdl_ts_assemble(x, new_data, ...)
}

# Bootstrap each model of a combination from the same time points, as the
# combination's own innovations can't be given to them.
bootstrap_combination <- function(x, new_data, block_size = NULL) {
  if (!inherits(x$fit, "model_combination") || !is.null(new_data[[".innov"]])) return(x)
  models <- x$fit$models
  draws <- attr(x, "bootstrap_draws") %||% bootstrap_draw_times(models, new_data, block_size)
  x$fit$models <- lapply(models, function(m) {
    structure(mdl_wrap(m, c("mdl_bootstrap_iid", "mdl_ts_sim")), bootstrap_draws = draws)
  })
  x
}

# Add innovations resampled from the model's residuals to the replicated
# new_data, using the time points drawn jointly with other models if given.
# Innovations already in new_data are kept.
bootstrap_innovations <- function(x, new_data, block_size = NULL) {
  if (!is.null(new_data[[".innov"]]) || is_null_model(x) || inherits(x$fit, "model_combination")) {
    return(new_data)
  }
  draws <- attr(x, "bootstrap_draws") %||% bootstrap_draw_times(list(x), new_data, block_size)
  if (length(draws) != NROW(new_data)) {
    abort("The jointly bootstrapped time points don't match the data being simulated.")
  }
  res <- bootstrap_residuals(x)
  pos <- vec_match(draws, res$index)
  new_data$.innov <- if (is.matrix(res$resid)) res$resid[pos, , drop = FALSE] else res$resid[pos]
  new_data
}

# Shared setup for generate.mdl_ts()/generate.mdl_bootstrap_iid()/
# generate.mdl_bootstrap_block(): establishes the RNG state (the caller is
# responsible for restoring the returned RNGstate via on.exit() once it has
# `seed`, since on.exit() must be registered in the caller's own frame),
# resolves/replicates new_data, and reseeds lag()'s short term memory.
generate_mdl_ts_setup <- function(x, new_data, h, times, seed) {
  RNGstate <- NULL
  if (!is.null(seed)) {
    RNGstate <- use_seed(seed)
  }

  if(is.null(new_data)){
    new_data <- make_future_data(x$data, h)
  }
  check_transformation_data(x, new_data)

  # Reseed lag()'s short term memory and trend()'s origin from this fit.
  restore_model_state(x)

  new_data <- replicate_new_data(new_data, times)

  list(new_data = new_data, RNGstate = RNGstate)
}

# Shared assembly for generate.mdl_ts()/generate.mdl_bootstrap_iid()/
# generate.mdl_bootstrap_block(): computes specials, simulates from the
# fitted model (using new_data$.innov if a modifier has populated it), and
# back-transforms the result. Kept as a single shared step so it's never
# duplicated across model modifiers.
generate_mdl_ts_assemble <- function(x, new_data, ...) {
  # Compute specials with new_data, which a model's checks would reject if
  # there is nothing to simulate (such as for an empty mable, #313).
  specials <- NULL
  if(NROW(new_data) > 0) {
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
  }

  .sim <- generate(x[["fit"]], new_data = new_data, specials = specials, ...)
  .sim_cols <- setdiff(names(.sim), names(new_data))

  # TODO: Breaking change, .sim should be response variable name
  # For now, only do this with multivariate models but this change will be made for v1.0.0
  resp_vars <- vapply(x$response, expr_name, character(1L), USE.NAMES = FALSE)
  if (length(resp_vars) > 1) {
    .sim_cols <- resp_vars
    .sim[resp_vars] <- split_cols(.sim$.sim)
    .sim$.sim <- NULL
  }

  # Back-transform forecast distributions
  bt <- map(x$transformation, function(x){
    invert_transformation(bind_transformation_data(x, new_data))
  })

  .sim[.sim_cols] <- .mapply(function(f, x) f(x), list(bt, .sim[.sim_cols]), NULL)
  .sim
}

# Demeaned innovation residuals for each time point of the model's data (NA
# where unavailable).
bootstrap_residuals <- function(x) {
  res <- residuals(x$fit, type = "innovation")
  res <- if (is.matrix(res)) {
    sweep(res, 2L, colMeans(res, na.rm = TRUE))
  } else {
    res - mean(res, na.rm = TRUE)
  }
  list(index = x$data[[index_var(x$data)]], resid = res)
}

# Draw the time points whose residuals are used as innovations for each row of
# the replicated `new_data`, shared by all `models`. Only time points with
# residuals for every model are drawn, and a block bootstrap draws blocks of
# `block_size` consecutive observed time points (not joined across gaps).
bootstrap_draw_times <- function(models, new_data, block_size = NULL, call = caller_env()) {
  res <- lapply(models, bootstrap_residuals)
  idx <- lapply(res, `[[`, "index")
  if (length(unique(lapply(idx, vec_ptype))) > 1L) {
    abort("Models bootstrapped jointly must have the same type of time index.", call = call)
  }
  observed <- lapply(res, function(r) {
    if (is.matrix(r$resid)) stats::complete.cases(r$resid) else !is.na(r$resid)
  })
  times <- Reduce(function(a, b) vec_slice(a, vec_in(a, b)), idx)
  ok <- Reduce(`&`, Map(function(i, o) o[vec_match(times, i)], idx, observed))

  if (length(models) > 1L) {
    shared <- vec_slice(times, ok)
    differs <- !all(map2_lgl(idx, observed, function(i, o) vec_size(i[o]) == vec_size(shared)))
    if (!any(ok)) {
      abort(c(
        "Models being bootstrapped jointly have no time points with residuals in common.",
        "i" = "Joint bootstrap sampling draws the same historical time point for every model."
      ), call = call)
    }
    if (differs) {
      warn(c(
        "Models being bootstrapped jointly have residuals for different time points.",
        "i" = "Joint bootstrap sampling has been restricted to their overlapping period."
      ))
    }
  } else if (!any(ok)) {
    abort("The model has no residuals to bootstrap.", call = call)
  }

  pos <- if (is.null(block_size)) {
    valid <- which(ok)
    valid[sample.int(length(valid), NROW(new_data), replace = TRUE)]
  } else {
    if (any(vapply(models, function(x) any(has_gaps(x$data)$.gaps), logical(1L)))) {
      abort("Residuals must be regularly spaced without gaps to use a block bootstrap method.", call = call)
    }
    if (length(unique(lapply(models, function(x) interval(x$data)))) > 1L) {
      abort("Models bootstrapped jointly with blocks must have the same time interval.", call = call)
    }
    # Blocks can start where the next `block_size` time points are all observed
    n_missing <- c(0L, cumsum(!ok))
    m <- length(ok)
    starts <- which(n_missing[seq_len(max(m - block_size + 1L, 0L)) + block_size] ==
                      n_missing[seq_len(max(m - block_size + 1L, 0L))])
    if (length(starts) == 0L) {
      runs <- rle(ok)
      abort(c(
        sprintf("Can't draw blocks of %d consecutive residuals for the block bootstrap.", block_size),
        "i" = sprintf(
          "There are at most %d consecutive time points with residuals%s.",
          max(c(0L, runs$lengths[runs$values])),
          if (length(models) > 1L) " for every model" else ""
        ),
        "i" = "Use a smaller `block_size`."
      ), call = call)
    }
    kr <- tsibble::key_rows(new_data)
    vec_c(!!!lapply(lengths(kr), function(n) block_bootstrap(starts, block_size, size = n)), .ptype = integer())
  }
  vec_slice(times, pos)
}

# Set the (deprecated) `seed` argument, returning the prior RNG state to be
# restored on exit.
use_seed <- function(seed) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) stats::runif(1)
  RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  RNGstate
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

# Resample positions in blocks of `window_size`, starting from any of `starts`.
block_bootstrap <- function(starts, window_size, size) {
  n_blocks <- size%/%window_size + 2
  block_pos <- starts[sample.int(length(starts), n_blocks, replace = TRUE)]
  bx <- as.vector(outer(seq_len(window_size) - 1L, block_pos, `+`))
  start_from <- sample.int(window_size, 1)
  bx[seq(start_from, length.out = size)]
}
