# A data frame with the attributes of a mable (its keys and model columns), as
# opposed to a model group, which has neither.
is_mable_shaped <- function(x){
  is.data.frame(x) && is.data.frame(attr(x, "key")) && !is.null(attr(x, "model"))
}

# Mables saved with fabletools <= 0.8.0 lead with the `mdl_df` class, which is
# now used for model groups.
is_legacy_mable <- function(x){
  cls <- class(x)
  is_mable_shaped(x) && inherits(x, "mdl_df") &&
    match("mdl_df", cls) < match("mbl_df", cls, length(cls) + 1L)
}

# Upgrade the class of a mable saved with fabletools <= 0.8.0
upgrade_mable <- function(x){
  if(!is_legacy_mable(x)) return(x)
  class(x) <- c("mbl_df", "mdl_df", setdiff(class(x), c("mbl_df", "mdl_df")))
  x
}

# Fables saved with fabletools <= 0.8.0 have the `fbl_ts` class without `fbl_df`
upgrade_fable <- function(x){
  if(inherits(x, "fbl_df")) return(x)
  cls <- class(x)
  pos <- match("fbl_ts", cls)
  class(x) <- append(cls, "fbl_df", after = pos - 1L)
  x
}

# A method for fables saved with fabletools <= 0.8.0, which upgrades the fable
# and dispatches again
legacy_fable_method <- function(generic, env = caller_env()){
  generic <- ensym(generic)
  fmls <- formals(args(eval(generic, env)))
  args <- set_names(syms(names(fmls)), names(fmls))
  names(args)[c(1L, match("...", names(args), 0L))] <- ""
  x <- sym(names(fmls)[1L])
  args[[1L]] <- expr(upgrade_fable(!!x))
  new_function(
    fmls,
    expr({
      if(inherits(!!x, "fbl_df")) return(NextMethod())
      (!!generic)(!!!args)
    }),
    env = env
  )
}

#' @export
accuracy.fbl_ts <- legacy_fable_method(accuracy)
#' @export
as_fable.fbl_ts <- legacy_fable_method(as_fable)
#' @export
as_tibble.fbl_ts <- legacy_fable_method(as_tibble)
#' @export
as_tsibble.fbl_ts <- legacy_fable_method(as_tsibble)
#' @export
distribution_var.fbl_ts <- legacy_fable_method(distribution_var)
#' @export
response_vars.fbl_ts <- legacy_fable_method(response_vars)
#' @export
hilo.fbl_ts <- legacy_fable_method(hilo)
#' @export
forecast.fbl_ts <- legacy_fable_method(forecast)
#' @export
fill_gaps.fbl_ts <- legacy_fable_method(fill_gaps)
#' @export
select.fbl_ts <- legacy_fable_method(select)
#' @export
rename.fbl_ts <- legacy_fable_method(rename)
#' @export
relocate.fbl_ts <- legacy_fable_method(relocate)
#' @export
transmute.fbl_ts <- legacy_fable_method(transmute)
#' @export
summarise.fbl_ts <- legacy_fable_method(summarise)
#' @export
group_by.fbl_ts <- legacy_fable_method(group_by)
#' @export
ungroup.fbl_ts <- legacy_fable_method(ungroup)
#' @export
dplyr_row_slice.fbl_ts <- legacy_fable_method(dplyr_row_slice)
#' @export
dplyr_col_modify.fbl_ts <- legacy_fable_method(dplyr_col_modify)
#' @export
dplyr_reconstruct.fbl_ts <- function(data, template){
  if(inherits(template, "fbl_df")) return(NextMethod())
  dplyr_reconstruct(data, upgrade_fable(template))
}
