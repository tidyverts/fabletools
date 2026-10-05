#' Run a hypothesis test from a mable
#' 
#' This function will return the results of a hypothesis test for each model in 
#' the mable.
#' 
#' @param x A mable.
#' @param ... Arguments for model methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' 
#' olympic_running %>%
#'   model(lm = TSLM(log(Time) ~ trend())) %>% 
#'   hypothesize()
#' 
#' @rdname hypothesize.mbl_df
#' @importFrom generics hypothesize
#' @export
hypothesize.mbl_df <- mbl_df_method(hypothesize, ".hypothesis")

#' @export
hypothesize.mdl_df <- mdl_df_method(hypothesize)

#' @export
hypothesize.mdl_lst <- mdl_lst_method(hypothesize)

#' @param tests a list of test functions to perform on the model
#' @rdname hypothesize.mbl_df
#' @export
hypothesize.mdl_ts <- function(x, tests = list(), ...){
  if(is_function(tests)){
    tests <- list(tests)
  }
  vctrs::vec_rbind(
    !!!map(tests, calc, x$fit, ...),
    .names_to = ".test"
  )
}