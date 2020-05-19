#' Extend a fitted model with new data
#'
#' Extend the length of data used to fit a model and update the parameters to 
#' suit this new data.
#'
#' @param object An object (such as a model) which can be extended with additional data.
#' @param ... Additional arguments passed on to stream methods.
#'
#' @rdname stream
#' @export
stream <- function(object, ...){
  UseMethod("stream")
}

#' @param new_data A dataset of the same structure as was used to fit the model.
#' 
#' @rdname stream
#' @export
stream.mdl_df <- function(object, new_data, ...){
  mdls <- mable_vars(object)
  new_data <- bind_new_data(object, new_data)
  object %>% 
    dplyr::mutate_at(vars(!!!mdls),
                     stream, new_data[["new_data"]], ...)
}

#' @export
stream.lst_mdl <- function(object, new_data, ...){
  add_class(map2(object, new_data, stream, ...), class(object))
}

#' @export
stream.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$add_data(new_data)
  specials <- parse_model_rhs(object$model)
  object$model$remove_data()
  
  resp <- map2(object$response, object$transformation, 
       function(y, t){
         eval_tidy(expr(t(!!y)), new_data)
       }
  )
  new_data <- new_data[index_var(new_data)]
  new_data[measured_vars(object$data)] <- resp
  
  object$fit <- stream(object[["fit"]], new_data, specials = specials, ...)
  object$data <- bind_rows(object$data, select(new_data, !!!syms(colnames(object$data))))
  object
}