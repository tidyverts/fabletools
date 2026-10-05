# A `mdl_df` is a joint group of models: a data frame whose rows are series
# and whose columns are model columns (`mdl_lst`, or nested `mdl_df`), cut
# from the same mable. Unlike a mable (`mbl_df`) it has no key of its own; row
# identity is positional, inherited from the mable it came from.
#
# While mables still carry "mdl_df" as a trailing class (dropped in v1.1.0),
# any `.mdl_df` method without a `.mbl_df` counterpart would also catch
# mables, so those methods defer to `NextMethod()` for mables.
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
  inherits(x, "mdl_df") && !is_mable(x)
}

is_model_col <- function(x){
  inherits(x, "mdl_lst") || is_mdl_df(x)
}

# The response variable(s) of a model column
model_col_response <- function(x){
  if(is_mdl_df(x)) response_vars(x) else response_vars(x[[1]])
}

#' @export
cbind.mdl_lst <- function(..., deparse.level = 1){
  dots <- list(...)
  if(any(map_lgl(dots, is_mable))) {
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

#' @export
cbind.mdl_df <- cbind.mdl_lst

#' @export
rbind.mdl_df <- function(..., deparse.level = 1){
  dots <- list(...)
  if(any(map_lgl(dots, is_mable))) {
    return(base::rbind.data.frame(..., deparse.level = deparse.level))
  }
  vec_rbind(!!!unname(dots))
}

#' @export
vec_ptype2.mdl_df.mdl_df <- function(x, y, ...){
  if(!setequal(names(x), names(y))) {
    abort("Can't combine model groups with different model columns.")
  }
  new_mdl_df(vctrs::tib_ptype2(x, y, ...))
}

#' @export
vec_cast.mdl_df.mdl_df <- function(x, to, ...){
  if(!setequal(names(x), names(to))) {
    abort("Can't convert between model groups with different model columns.")
  }
  new_mdl_df(vctrs::tib_cast(x, to, ...))
}

#' @export
mable_vars.mdl_df <- function(x){
  names(x)
}

#' @export
response_vars.mdl_df <- function(x){
  # Unlike a mable, a model group's models needn't share a response.
  unique(unlist(map(unclass(x), model_col_response), use.names = FALSE))
}

#' @export
model_sum.mdl_df <- function(x){
  if(is_mable(x)) return(NextMethod())
  # One summary per row, combining the summaries of each model in the group.
  row_sum <- map(unclass(x), function(col) {
    if(is_mdl_df(col)) paste0("<", model_sum(col), ">") else map_chr(col, model_sum)
  })
  if(length(row_sum) == 0) return(character(NROW(x)))
  do.call(paste, c(unname(row_sum), sep = " + "))
}

type_sum.mdl_df <- function(x){
  if(is_mable(x)) return(NextMethod())
  "models"
}

tbl_sum.mdl_df <- function(x){
  if(is_mable(x)) return(NextMethod())
  c(`A model group` = paste(map_chr(dim(x), big_mark), collapse = " x "))
}

# Splice the data frame results of `mdl_df` columns (from `dispatch_mdl_df()`)
# into top-level columns named `<group>$<model>`, so
# every model's results can be pivoted into a single `.model` column.
unpack_model_results <- function(x){
  out <- map2(x, names(x), function(col, nm) {
    if(!is.data.frame(col)) return(set_names(list(col), nm))
    res <- unpack_model_results(unclass(col))
    set_names(res, paste0(nm, "$", names(res)))
  })
  unlist(unname(out), recursive = FALSE) %||% list()
}
