# Method factories for generics that dispatch through the model containers. A
# mable (`mbl_df`) holds model columns, each either a model group (`mdl_df`) or
# a list of models (`mdl_lst`), whose elements are the models (`mdl_ts`). The
# `mdl_ts` method holds the model specific logic and is written by hand, while
# the container methods above it are created here so that every generic
# traverses the containers in the same way:
#
# * `mdl_lst`: the generic is applied to each model (in parallel if enabled),
#   giving a list with one result per row.
# * `mdl_df`: the generic is applied to each model column, giving a tibble of
#   per-model results.
# * `mbl_df`: the generic is applied to each model column, and the results are
#   returned in long form with a `.model` column. Models of a group are named
#   `<group>$<model>`.
#
# Generics which modify the models (`modify = TRUE`, such as refit()) instead
# return the same container with its models replaced by the results.
#
# Generics can also take a `new_data` argument holding data for each series.
# A mable binds it to its rows with `bind_new_data()`, and the data for each
# row is passed down the containers to the corresponding model.
#
# The generated methods have the formals of their generic, and call the generic
# on the next level down. A container method can still be written by hand when
# it needs behaviour of its own (such as joint estimation across series), and
# can use the `dispatch_*()` engines below directly.

# Create a method whose body passes its arguments to `fn`, along with the
# generic as `.f` and the options in `...`. The generic is looked up when the
# method is called. `.formals` are added after the dispatched object, and are
# only passed on if referenced by the options.
new_dispatch_method <- function(generic, fn, ..., .formals = NULL, env = caller_env(2)) {
  fmls <- formals(args(eval(generic, env)))
  fmls <- fmls[setdiff(names(fmls), names(.formals))]
  args <- set_names(syms(names(fmls)), names(fmls))
  # The dispatched object and `...` are passed unnamed.
  names(args)[c(1L, match("...", names(args), 0L))] <- ""
  opts <- list2(...)
  opts <- opts[!vapply(opts, is.null, logical(1L))]
  new_function(
    args = c(fmls[1L], .formals, fmls[-1L]),
    body = expr((!!fn)(!!!args, .f = !!generic, !!!opts)),
    env = env
  )
}

new_data_formal <- function(new_data = c("none", "optional", "required")) {
  switch(arg_match(new_data),
    none = NULL,
    optional = list(new_data = NULL),
    required = alist(new_data = )
  )
}

#' Create a method for applying a generic to the models of a mable
#'
#' @param generic The generic, as a symbol.
#' @param values_to The name of the temporary column holding nested results.
#' @param unnest How the results are combined: as a tibble (`"tbl"`), as a
#' tsibble keyed by the mable's keys and `.model` (`"tsibble"`), or a function
#' of the pivoted results, the name of the results column and the keys.
#' @param new_data Whether the method has a `new_data` argument, which is bound
#' to the rows of the mable and passed to each model.
#' @param modify If `TRUE`, the generic modifies the models and the method
#' returns the mable with its models replaced.
#'
#' @noRd
mbl_df_method <- function(generic, values_to = ".result", unnest = c("tbl", "tsibble"),
                          new_data = c("none", "optional", "required"),
                          modify = FALSE) {
  new_data <- arg_match(new_data)
  if (!is.function(unnest)) unnest <- arg_match(unnest)
  opts <- if (!modify) list(.values_to = values_to, .unnest = unnest)
  new_dispatch_method(
    ensym(generic),
    if (modify) quote(dispatch_mbl_df_modify) else quote(dispatch_mbl_df),
    !!!opts,
    .new_data = if (new_data != "none") quote(new_data),
    .formals = new_data_formal(new_data)
  )
}

mdl_df_method <- function(generic, modify = FALSE) {
  new_dispatch_method(
    ensym(generic),
    if (modify) quote(dispatch_mdl_df_modify) else quote(dispatch_mdl_df)
  )
}

mdl_lst_method <- function(generic, new_data = FALSE, modify = FALSE) {
  new_dispatch_method(
    ensym(generic),
    if (modify) quote(dispatch_mdl_lst_modify) else quote(dispatch_mdl_lst),
    .new_data = if (new_data) quote(new_data),
    .formals = new_data_formal(if (new_data) "optional" else "none")
  )
}

# Apply `.f` to each model column of a mable, returning the results in long form
# with a `.model` column.
#
# `.new_data` is bound to the rows of the mable, and a mable which already has
# a `new_data` column (from `bind_new_data()`) is used as is. `.unnest` is
# either `"tbl"`, `"tsibble"`, or a function of the pivoted results, the name
# of the results column and the keys of the output. `.reserved` are the names
# that can't already be used by the keys or `new_data` (see
# `check_reserved_names()`).
dispatch_mbl_df <- function(x, ..., .f, .values_to = ".result",
                            .unnest = c("tbl", "tsibble"), .new_data = NULL,
                            .reserved = ".model") {
  if (NROW(x) == 0) {
    return(dispatch_mbl_df_empty(
      x, ..., .f = .f, .values_to = .values_to, .unnest = .unnest,
      .new_data = .new_data, .reserved = .reserved
    ))
  }
  check_reserved_names(x, .new_data, key = .reserved, call = caller_env())
  if (!is.null(.new_data)) {
    x <- bind_new_data(x, .new_data)
  }
  mdls <- mable_vars(x)
  kv <- c(key_vars(x), ".model")
  x <- as_tibble(x)
  res <- unpack_model_results(
    lapply(x[mdls], call_with_new_data, .f = .f, new_data = x[["new_data"]], ...)
  )
  x <- vec_cbind(
    x[setdiff(names(x), c(mdls, "new_data"))],
    tibble::new_tibble(res, nrow = NROW(x))
  )
  x <- tidyr::pivot_longer(x, all_of(names(res)), names_to = ".model", values_to = .values_to)
  if (is.function(.unnest)) {
    return(.unnest(x, .values_to, kv))
  }
  switch(arg_match(.unnest),
    tbl = unnest_tbl(x, .values_to),
    tsibble = unnest_tsbl(x, .values_to, parent_key = kv)
  )
}

# The results of a mable without any models are structured by applying `.f` to
# its prototype model (see `mable_ptype()`) for a placeholder series, which is
# then removed from the results (#313).
dispatch_mbl_df_empty <- function(x, ..., .new_data = NULL) {
  ptype <- mable_ptype(x)
  if (is.null(ptype)) {
    abort(
      "Can't compute results for a mable without any models, as the structure of its models is unknown.",
      call = caller_env(2)
    )
  }
  mdls <- mable_vars(x)
  kv <- key_vars(x)
  tbl <- vec_init(as_tibble(x)[setdiff(names(x), "new_data")], 1L)
  tbl[mdls] <- lapply(tbl[mdls], placeholder_model_col, ptype)
  if (!is.null(.new_data)) {
    tbl[["new_data"]] <- list(unkey_tsibble(.new_data[0, setdiff(names(.new_data), kv)]))
  }
  x <- build_mable_meta(
    tbl, key_data = dplyr::group_data(dplyr::group_by(tbl, !!!syms(kv))),
    model = mdls, response = response_vars(x)
  )
  vec_slice(dispatch_mbl_df(x, ...), 0L)
}

# Replace the models of a model column with `ptype`. Subclasses of `mdl_lst`
# (such as reconciliation) are dropped as they apply to the models it replaces.
placeholder_model_col <- function(x, ptype) {
  if (is_mdl_df(x)) {
    return(new_mdl_df(lapply(as.list(x), placeholder_model_col, ptype)))
  }
  new_mdl_lst(rep(list(ptype), vec_size(x)))
}

# Apply `.f` to each model column of a mable, replacing them with the results.
dispatch_mbl_df_modify <- function(x, ..., .f, .new_data = NULL) {
  if (!is.null(.new_data)) {
    x <- bind_new_data(x, .new_data)
  }
  mdls <- mable_vars(x)
  new_data <- x[["new_data"]]
  x[["new_data"]] <- NULL
  dplyr::dplyr_col_modify(
    x,
    lapply(as_tibble(x)[mdls], call_with_new_data, .f = .f, new_data = new_data, ...)
  )
}

dispatch_mdl_df <- function(x, ..., .f) {
  tibble::new_tibble(lapply(unclass(x), .f, ...), nrow = NROW(x))
}

dispatch_mdl_df_modify <- function(x, ..., .f) {
  new_mdl_df(lapply(unclass(x), .f, ...))
}

# `.new_data` holds the data for each model.
dispatch_mdl_lst <- function(x, ..., .f, .new_data = NULL) {
  args <- list(vec_data(x))
  if (!is.null(.new_data)) {
    args$new_data <- .new_data
  }
  exec(mapply_maybe_parallel, .f, !!!args, MoreArgs = list2(...))
}

dispatch_mdl_lst_modify <- function(x, ..., .f, .new_data = NULL) {
  out <- dispatch_mdl_lst(x, ..., .f = .f, .new_data = .new_data)
  attributes(out) <- attributes(x)
  out
}

# Pass `new_data` to `.f` only when there is some, so that methods use their
# own default otherwise.
call_with_new_data <- function(x, .f, new_data = NULL, ...) {
  if (is.null(new_data)) .f(x, ...) else .f(x, new_data = new_data, ...)
}

# Shared checks for generics producing results for future time points, which
# can be specified with either `new_data` or `h`. Returns the horizon to use.
check_horizon <- function(new_data, h) {
  if (!is.null(h) && !is.null(new_data)) {
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  h
}
