deprecate_lst_mdl <- function(f) {
  fsym <- ensym(f)
  fmls <- formals(args(f))
  args <- names(fmls)
  args <- `names<-`(syms(args), args)
  # Remove name of `...` if it exists
  arg_dots <- match("...", names(args), 0L)
  names(args)[arg_dots] <- ""

  rlang::new_function(
    args = fmls,
    body = rlang::expr({
      lifecycle::deprecate_soft(
          "0.7.0",
          I("The `lst_mdl` class"),
          I("the `mdl_lst` class for lists of models")
        )
        (!!fsym)(!!!args)
      }
    ),
    env = rlang::caller_env()
  )
}