# Modifiers changing how a model simulates sample paths, and forecasts from them
mdl_sim_classes <- c("mdl_bootstrap_iid", "mdl_bootstrap_block", "mdl_ts_sim")

# Modifiers adjusting a model's forecasts, which are kept outside of the
# modifiers above so that they apply whichever is wrapped first.
mdl_adjust_classes <- "mdl_conformal_scp"

# Wrap a model (or model specification) with extra classes and store `...` as
# attributes. Wrapping again with the same modifier replaces its settings.
mdl_wrap <- function(x, .class, ...) {
  # NULL models (from failed estimation) give NA forecasts however they're made
  if (inherits(x, "mdl_ts") && is_null_model(x)) return(x)
  # Model definitions are R6 objects, so copy them to avoid modifying the original
  if (inherits(x, "R6")) x <- clone_model_definition(x)
  cls <- setdiff(class(x), .class)
  n_adjust <- if (any(.class %in% mdl_adjust_classes)) 0L else match(FALSE, cls %in% mdl_adjust_classes) - 1L
  x <- structure(x, class = c(cls[seq_len(n_adjust)], .class, cls[seq_along(cls) > n_adjust]))
  attrs <- list(...)
  for (nm in names(attrs)) attr(x, nm) <- attrs[[nm]]
  x
}

# Wrap a model column with a class for modifying all of its models jointly,
# keeping its existing classes (such as reconciliation).
mdl_wrap_col <- function(x, .class, ...) {
  x <- structure(x, class = c(.class, setdiff(class(x), .class)))
  attrs <- list(...)
  for (nm in names(attrs)) attr(x, nm) <- attrs[[nm]]
  x
}

# Apply `.f` to each model of a model column, keeping the column's classes and
# attributes.
map_models <- function(x, .f, ...) {
  out <- lapply(vec_data(x), .f, ...)
  attributes(out) <- attributes(x)
  out
}

abort_invalid_modifier <- function(model, fn, call = caller_env()) {
  abort(sprintf(
    "`%s()` must be applied to a model column (such as within `mutate()`), a model group, or a model specification, not <%s>.",
    fn, class(model)[1]
  ), call = call)
}

# Check that a modifier's argument is a single positive whole number
check_count <- function(x, arg = caller_arg(x), call = caller_env(), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(x))
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != trunc(x)) {
    abort(sprintf("`%s` must be a single positive whole number.", arg), call = call)
  }
  invisible(x)
}
