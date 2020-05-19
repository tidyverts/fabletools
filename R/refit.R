#' Refit a mable to a new dataset
#' 
#' Applies a fitted model to a new dataset. For most methods this can be done
#' with or without re-estimation of the parameters.
#' 
#' @param object A mable.
#' @param new_data A tsibble dataset used to refit the model.
#' @param ... Additional optional arguments for refit methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' 
#' fit <- as_tsibble(mdeaths) %>% 
#'   model(ETS(value ~ error("M") + trend("A") + season("A")))
#' fit %>% report()
#'
#' fit %>% 
#'   refit(as_tsibble(fdeaths)) %>% 
#'   report(reinitialise = TRUE)
#' }
#' 
#' @rdname refit
#' @export
refit.mdl_df <- function(object, new_data, ...){
  mdls <- syms(mable_vars(object))
  new_data <- bind_new_data(object, new_data)
  
  object %>% 
    dplyr::mutate_at(vars(!!!mdls),
                     refit, new_data[["new_data"]], ...)
}

#' @export
refit.lst_mdl <- function(object, new_data, ...){
  `class<-`(map2(object, new_data, refit, ...), class(object))
}

#' @rdname refit
#' @export
refit.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$stage <- "refit"
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)
  object$model$remove_data()
  object$model$stage <- NULL
  
  resp <- map2(seq_along(object$response), object$response, function(i, resp){
    expr(object$transformation[[!!i]](!!resp))
  }) %>% 
    set_names(map_chr(object$response, as_string))
  
  new_data <- transmute(new_data, !!!resp)
  object$fit <- refit(object[["fit"]], new_data, specials = specials, ...)
  object$data <- new_data
  object
}