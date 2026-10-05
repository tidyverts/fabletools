#' Refit a mable to a new dataset
#' 
#' Applies a fitted model to a new dataset. For most methods this can be done
#' with or without re-estimation of the parameters.
#' 
#' @param object A mable.
#' @param new_data A tsibble dataset used to refit the model.
#' @param ... Additional optional arguments for refit methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' 
#' fit <- as_tsibble(mdeaths) %>% 
#'   model(ETS(value ~ error("M") + trend("A") + season("A")))
#' fit %>% report()
#'
#' fit %>% 
#'   refit(as_tsibble(fdeaths)) %>% 
#'   report(reinitialise = TRUE)
#' 
#' @rdname refit
#' @export
refit.mbl_df <- mbl_df_method(refit, new_data = "required", modify = TRUE)

#' @export
refit.mdl_df <- mdl_df_method(refit, modify = TRUE)

#' @export
refit.mdl_lst <- mdl_lst_method(refit, new_data = TRUE, modify = TRUE)
#' @export
refit.lst_mdl <- deprecate_lst_mdl(refit.mdl_lst)

#' @rdname refit
#' @export
refit.mdl_ts <- function(object, new_data, ...){
  # Reseed lag()'s short term memory from this fit's own snapshot.
  recent_data <- attr(object, "recent_data")
  object$model$recent_data <- recent_data

  # Compute specials with new_data
  object$model$stage <- "refit"
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)
  object$model$remove_data()
  object$model$stage <- NULL

  # new_data is the complete replacement history, so its tail is the new window.
  if (NROW(recent_data) > 0) {
    attr(object, "recent_data") <- utils::tail(new_data, NROW(recent_data))
  }

  check_transformation_data(object, new_data)
  params <- model_transformation_params(object)
  trans <- map(object$transformation, bind_transformation_data, new_data)
  resp <- map2(seq_along(object$response), object$response, function(i, resp){
    expr(trans[[!!i]](!!resp))
  }) %>%
    set_names(map_chr(object$response, as_string))

  # Equivalent to transmute(new_data, !!!resp), but much cheaper for the
  # repeated refits in rolling-origin loops (e.g. conformal_scp()).
  resp <- lapply(resp, eval_tidy, data = new_data, env = environment())
  param_data <- as.list(new_data)[params]
  new_data <- new_data[c(key_vars(new_data), index_var(new_data))]
  new_data[names(resp)] <- resp
  object$fit <- refit(object[["fit"]], new_data, specials = specials, ...)
  # Store time-varying transformation parameters alongside the response (#382)
  new_data[params] <- param_data
  object$data <- new_data
  object
}