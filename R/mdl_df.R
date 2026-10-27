# A `mdl_df` is a joint group of models: a data frame whose rows are series
# and whose columns are model columns (`mdl_lst`, or nested `mdl_df`), cut
# from the same mable. Unlike a mable (`mbl_df`) it has no key of its own; row
# identity is positional, inherited from the mable it came from.
#
# While mables still carry "mdl_df" as a trailing class (dropped in v1.1.0),
# and mables saved with fabletools <= 0.8.0 lead with it, `.mdl_df` methods
# can also catch mables. These are dispatched again as mables instead.
new_mdl_df <- function(x = list()){
  x <- as.list(x)
  if(any(names2(x) == "")) {
    abort("All model columns of a model group must be named.")
  }
  if(anyDuplicated(names(x))) {
    abort("Model columns of a model group must have unique names.")
  }
  if(!all(map_lgl(x, is_model_col))) {
    abort("A model group can only contain model columns (`mdl_lst` or `mdl_df`).")
  }
  n <- unique(map_int(x, vec_size))
  if(length(n) > 1) {
    abort("Model columns of a model group must contain the same number of models.")
  }
  tibble::new_tibble(x, nrow = if(length(n)) n else 0L, class = "mdl_df")
}

is_mdl_df <- function(x){
  inherits(x, "mdl_df") && !is_mable(x) && !is_mable_shaped(x)
}

is_model_col <- function(x){
  inherits(x, "mdl_lst") || is_mdl_df(x)
}

# The response variable(s) of a model column, or NULL if it contains no models
model_col_response <- function(x){
  if(is_mdl_df(x)) return(response_vars(x))
  mdl <- detect_model(x)
  if(is.null(mdl)) NULL else response_vars(mdl)
}

#' Model groups
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Model columns of a mable can be combined with `cbind()` into a model group:
#' a data frame of model columns (with the `mdl_df` class) holding several
#' models for each series. A model group is created within [dplyr::mutate()]
#' (e.g. `mutate(fit, grp = cbind(ets, arima))`), and is stored in the mable
#' as a single model column. `rbind()` stacks the rows of model groups with
#' the same model columns, which are matched by name.
#'
#' @details
#' Unnamed model columns are named after their expression, with `fit$ets`
#' named `ets`. Unnamed model groups are extended with their model columns,
#' while named model groups are nested within the new group, so
#' `cbind(grp, arima)` adds `arima` to the models of `grp`, and
#' `cbind(grp = grp, arima)` gives a group containing `grp` and `arima`. The
#' model columns must all contain the same number of models (one for each
#' series in the mable).
#'
#' When printed within a mable, a model group is shown as a column for each of
#' its models, under the name of the group (such as `grp$ets` followed by
#' `$arima`).
#'
#' Results computed from a mable include each model of a group, labelled in
#' the `.model` column by the string `"<group>$<model>"` (e.g. `"grp$ets"`).
#' This applies to
#' [forecast()], [generate()], [accuracy()], [fitted()], [residuals()],
#' [augment()], [glance()], [tidy()], [components()] and [hypothesize()].
#' These labels must be unique, so results can't be computed from a mable that
#' also has a model column named `grp$ets`.
#' Generics that modify the models, such as [refit()] and [stream()], return
#' the model group with its models replaced. Model groups can also be used
#' with:
#'
#' * [report()] and [equation()], for a mable with a single model.
#' * [tidyr::pivot_longer()] and [tidyr::gather()], which replace the model
#'   groups being pivoted with their models (named `<group>$<model>`).
#' * [simulate_iid()] and [conformal_scp()], which are applied to each model of
#'   the group.
#' * [bootstrap_iid()] and [bootstrap_block()], which sample jointly across
#'   every model of the group: the same historical time point (or block) is
#'   drawn for each model and series at a given step of each path, so their
#'   forecasts share the same resampled residuals. This includes the models of
#'   bootstrapped or reconciled columns within the group.
#'
#' The reconciliation functions (such as [reconcile_mint()]) can't be applied
#' to a model group. Instead apply them to the model columns before grouping
#' them.
#' [interpolate()] requires a mable with a single model column, which can't be
#' a model group.
#'
#' @param ... Model columns (`mdl_lst`) or model groups (`mdl_df`) to combine.
#' @param deparse.level For `cbind()`, how unnamed arguments are named. With
#' `1` (the default) unnamed symbols (or `fit$model`) are used as names, and
#' with `2` other expressions are also deparsed into names. With `0`, unnamed
#' model columns aren't allowed. Unused by `rbind()`.
#'
#' @return A model group, with the `mdl_df` class.
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(dplyr)
#' fit <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = TRUE) %>%
#'   model(ets = ETS(value), lm = TSLM(value ~ trend() + season()))
#'
#' # Group the models of each series
#' fit_grp <- fit %>%
#'   transmute(grp = cbind(ets, lm))
#' fit_grp
#'
#' # The results include each model of the group
#' fit_grp %>%
#'   forecast(h = "1 year")
#' accuracy(fit_grp)
#'
#' @name model_groups
#' @export
cbind.mdl_lst <- function(..., deparse.level = 1){
  dots <- list(...)
  if(any(map_lgl(dots, is_mable))) {
    if(any(map_lgl(dots, inherits, "mdl_lst"))) {
      abort(c(
        "Model columns can't be added to a mable with `cbind()`.",
        i = "Use `mutate()` to add model columns, e.g. `mutate(fit, ets2 = ets)`."
      ))
    }
    return(base::cbind.data.frame(..., deparse.level = deparse.level))
  }

  # cbind() is dispatched internally, so the names of unnamed arguments are
  # recovered from the caller's expressions (as with cbind.data.frame()).
  exprs <- as.list(substitute(list(...)))[-1L]
  nms <- names2(dots)
  cols <- list()
  for(i in seq_along(dots)) {
    x <- dots[[i]]
    if(!is_model_col(x)) {
      abort(sprintf(
        "Only model columns (`mdl_lst` or `mdl_df`) can be combined into a model group, not <%s>.",
        vec_ptype_full(x)
      ))
    }
    if(nms[i] == "" && is_mdl_df(x)) {
      # Unnamed model groups are extended with their component columns
      cols <- c(cols, as.list(x))
      next
    }
    if(nms[i] == "") {
      expr <- exprs[[i]]
      # Name `fit$ets` as `ets`, as it would be if used within the mable.
      if(is_call(expr, "$", n = 2) && is.symbol(expr[[3]])) expr <- expr[[3]]
      nms[i] <- if(deparse.level >= 1 && is.symbol(expr)) {
        as_string(expr)
      } else if(deparse.level == 2) {
        paste(deparse(exprs[[i]]), collapse = " ")
      } else {
        abort("All model columns being combined into a model group must be named.")
      }
    }
    cols <- c(cols, set_names(list(x), nms[i]))
  }
  new_mdl_df(cols)
}

#' @rdname model_groups
#' @export
cbind.mdl_df <- cbind.mdl_lst

#' @rdname model_groups
#' @export
rbind.mdl_df <- function(..., deparse.level = 1){
  dots <- list(...)
  if(any(map_lgl(dots, is_mable_shaped))) {
    return(base::rbind.data.frame(..., deparse.level = deparse.level))
  }
  vec_rbind(!!!unname(dots))
}

#' @export
vec_ptype2.mdl_df.mdl_df <- function(x, y, ...){
  if(is_legacy_mable(x) || is_legacy_mable(y)) return(vec_ptype2(upgrade_mable(x), upgrade_mable(y), ...))
  if(!setequal(names(x), names(y))) {
    abort("Can't combine model groups with different model columns.")
  }
  new_mdl_df(vctrs::tib_ptype2(x, y, ...))
}

#' @export
vec_cast.mdl_df.mdl_df <- function(x, to, ...){
  if(is_legacy_mable(x) || is_legacy_mable(to)) return(vec_cast(upgrade_mable(x), upgrade_mable(to), ...))
  if(!setequal(names(x), names(to))) {
    abort("Can't convert between model groups with different model columns.")
  }
  new_mdl_df(vctrs::tib_cast(x, to, ...))
}

# Model groups modified jointly (by the bootstrap modifiers) can only be
# combined with groups modified in the same way.
#' @rawNamespace S3method(vec_ptype2, mdl_df.mdl_df_bootstrap_iid, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df.mdl_df_bootstrap_block, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_iid.mdl_df, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_iid.mdl_df_bootstrap_iid, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_iid.mdl_df_bootstrap_block, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_block.mdl_df, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_block.mdl_df_bootstrap_iid, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_ptype2, mdl_df_bootstrap_block.mdl_df_bootstrap_block, vec_ptype2_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df.mdl_df_bootstrap_iid, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df.mdl_df_bootstrap_block, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_iid.mdl_df, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_iid.mdl_df_bootstrap_iid, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_iid.mdl_df_bootstrap_block, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_block.mdl_df, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_block.mdl_df_bootstrap_iid, vec_cast_mdl_df_modified)
#' @rawNamespace S3method(vec_cast, mdl_df_bootstrap_block.mdl_df_bootstrap_block, vec_cast_mdl_df_modified)
NULL

vec_ptype2_mdl_df_modified <- function(x, y, ...){
  if(!identical(class(x), class(y))) {
    vctrs::stop_incompatible_type(x, y, ..., details = "Model groups with different modifiers can't be combined.")
  }
  restore_mdl_df_modifier(vec_ptype2.mdl_df.mdl_df(x, y, ...), x)
}

vec_cast_mdl_df_modified <- function(x, to, ...){
  if(!identical(class(x), class(to))) {
    vctrs::stop_incompatible_cast(x, to, ..., details = "Model groups with different modifiers can't be combined.")
  }
  restore_mdl_df_modifier(vec_cast.mdl_df.mdl_df(x, to, ...), to)
}

restore_mdl_df_modifier <- function(x, from){
  class(x) <- class(from)
  attr(x, "block_size") <- attr(from, "block_size")
  x
}

#' @export
mable_vars.mdl_df <- function(x){
  if(is_legacy_mable(x)) return(mable_vars(upgrade_mable(x)))
  if(is_mable(x)) return(NextMethod())
  names(x)
}

#' @export
response_vars.mdl_df <- function(x){
  if(is_legacy_mable(x)) return(response_vars(upgrade_mable(x)))
  if(is_mable(x)) return(NextMethod())
  # Unlike a mable, a model group's models needn't share a response.
  unique(unlist(map(unclass(x), model_col_response), use.names = FALSE))
}

#' @export
model_sum.mdl_df <- function(x){
  if(is_legacy_mable(x)) return(model_sum(upgrade_mable(x)))
  if(is_mable(x)) return(NextMethod())
  # One summary per row, combining the summaries of each model in the group.
  row_sum <- map(unclass(x), function(col) {
    if(is_mdl_df(col)) paste0("<", model_sum(col), ">") else map_chr(col, model_sum)
  })
  if(length(row_sum) == 0) return(character(NROW(x)))
  do.call(paste, c(unname(row_sum), sep = " + "))
}

type_sum.mdl_df <- function(x){
  if(is_legacy_mable(x)) return(pillar::type_sum(upgrade_mable(x)))
  if(is_mable(x)) return(NextMethod())
  "models"
}

tbl_sum.mdl_df <- function(x){
  if(is_legacy_mable(x)) return(tibble::tbl_sum(upgrade_mable(x)))
  if(is_mable(x)) return(NextMethod())
  c(`A model group` = paste(map_chr(dim(x), big_mark), collapse = " x "))
}

# Modifiers only apply to model groups, not to mables (which can also have
# the `mdl_df` class)
check_model_group <- function(model, fn, call = caller_env()) {
  if(!is_mdl_df(model)) {
    abort(sprintf(
      "`%s()` must be applied to a model column, such as within `mutate()`, not to a mable.", fn
    ), call = call)
  }
}

# Apply a modifier to each model column of a model group
modify_model_group <- function(model, .f, ..., call = caller_env()) {
  check_model_group(model, as_string(substitute(.f)), call = call)
  out <- lapply(unclass(model), .f, ...)
  attributes(out) <- attributes(model)
  out
}

# The model columns of a mable, with model groups replaced by their columns
mable_model_cols <- function(x){
  unpack_model_results(as.list(x)[mable_vars(x)])
}

# Splice the data frame results of `mdl_df` columns (from `dispatch_mdl_df()`)
# into top-level columns named `<group>$<model>`, so
# every model's results can be pivoted into a single `.model` column.
unpack_model_results <- function(x, call = caller_env()){
  unpack <- function(x) {
    out <- map2(x, names(x), function(col, nm) {
      if(!is.data.frame(col)) return(set_names(list(col), nm))
      res <- unpack(unclass(col))
      set_names(res, paste0(nm, "$", names(res)))
    })
    unlist(unname(out), recursive = FALSE) %||% list()
  }
  out <- unpack(x)
  check_model_labels(names(out), names(x), call = call)
  out
}

# Models of a group are labelled `<group>$<model>`, which can match the name
# of another model column.
check_model_labels <- function(labels, cols, call = caller_env()){
  dup <- unique(labels[duplicated(labels)])
  if(length(dup) == 0L) return(invisible(labels))
  dup <- dup[1]
  grp <- sub("\\$.*", "", dup)
  abort(if(dup %in% cols && grp %in% cols) {
    sprintf("Model labels must be unique; `%s` is both a column and a model of group `%s`.", dup, grp)
  } else {
    sprintf("Model labels must be unique; `%s` labels more than one model.", dup)
  }, call = call)
}
