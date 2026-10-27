# Wrap a model with extra classes and store `...` as attributes.
# Building block for bootstrap_iid()/bootstrap_block()/simulate_iid().
mdl_wrap <- function(x, .class, ...) {
  # NULL models (from failed estimation) give NA forecasts however they're made
  if (inherits(x, "mdl_ts") && is_null_model(x)) return(x)
  # Model definitions are R6 objects, so copy them to avoid modifying the original
  if (inherits(x, "R6")) x <- clone_model_definition(x)
  x <- structure(x, class = c(.class, class(x)))
  attrs <- list(...)
  for (nm in names(attrs)) attr(x, nm) <- attrs[[nm]]
  x
}

# Check that a modifier's argument is a single positive whole number
check_count <- function(x, arg = caller_arg(x), call = caller_env(), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(x))
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != trunc(x)) {
    abort(sprintf("`%s` must be a single positive whole number.", arg), call = call)
  }
  invisible(x)
}
