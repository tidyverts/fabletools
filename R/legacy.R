# A data frame with the attributes of a mable (its keys and model columns), as
# opposed to a model data frame, which has neither.
is_mable_shaped <- function(x){
  is.data.frame(x) && is.data.frame(attr(x, "key")) && !is.null(attr(x, "model"))
}

# Mables saved with fabletools <= 0.8.0 lead with the `mdl_df` class, which is
# now used for model data frames.
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
  x <- sym(names(fmls)[1L])
  opts <- setdiff(names(fmls)[-1L], "...")
  # Omit defaults so that the generic evaluates its own for unsupplied arguments
  fmls[names(fmls) != "..."] <- list(missing_arg())
  new_function(
    fmls,
    expr({
      if(inherits(!!x, "fbl_df")) return(NextMethod())
      legacy_fable_dispatch(
        !!as_string(generic), !!generic, upgrade_fable(!!x), !!opts, environment(),
        dots = !!("..." %in% names(fmls))
      )
    }),
    env = env
  )
}

# Call `fn` on the upgraded fable `x` with the arguments supplied to `frame`,
# forwarding their expressions unevaluated so that tidy evaluation still works.
legacy_fable_dispatch <- function(name, fn, x, opts, frame, dots){
  opts <- opts[!vapply(opts, function(a) eval(call("missing", as.name(a)), frame), logical(1L))]
  quos <- lapply(opts, function(a) eval(call("enquo", as.name(a)), frame))
  args <- set_names(lapply(quos, quo_get_expr), opts)
  envs <- unique(lapply(quos[!vapply(args, is_syntactic_literal, logical(1L))], quo_get_env))
  if(length(envs) > 1) {
    # Arguments from different environments are evaluated instead
    args <- set_names(syms(opts), opts)
    envs <- list(frame)
  }
  mask <- env(if(length(envs)) envs[[1]] else frame, .legacy_fable = x)
  mask[[name]] <- fn
  call <- as.call(c(sym(name), quote(.legacy_fable), if(dots) quote(...), args))
  forward <- new_function(if(dots) alist(... = ), call, mask)
  eval(if(dots) call2(forward, quote(...)) else call2(forward), frame)
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
