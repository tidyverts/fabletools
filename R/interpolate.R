#' Interpolate missing values
#' 
#' Uses a fitted model to interpolate missing values from a dataset.
#' 
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' 
#' # The fastest running times for the olympics are missing for years during 
#' # world wars as the olympics were not held.
#' olympic_running
#' 
#' olympic_running %>% 
#'   model(TSLM(Time ~ trend())) %>% 
#'   interpolate(olympic_running)
#' 
#' @rdname interpolate
#' @export
interpolate.mbl_df <- function(object, new_data, ...){
  mdls <- mable_vars(object)
  if(length(mdls) > 1 || is_mdl_df(object[[mdls]])){
abort("Interpolation can only be done using one model. 
Please use select() to choose the model to interpolate with.")
  }

  # The interpolated data has the same structure as `new_data`, and so the
  # `.model` column identifying the only model isn't added.
  dispatch_mbl_df(
    object, ..., .f = interpolate, .new_data = new_data,
    .values_to = ".interpolated", .reserved = NULL,
    .unnest = function(x, col, key) {
      x[[".model"]] <- NULL
      unnest_tsbl(x, col, parent_key = setdiff(key, ".model"))
    }
  )
}

#' @export
interpolate.mdl_df <- mdl_df_method(interpolate)

#' @export
interpolate.mdl_lst <- mdl_lst_method(interpolate, new_data = TRUE)

#' @rdname interpolate
#' @export
interpolate.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$stage <- "interpolate"
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model),
                       error = function(e){
                         abort(sprintf(
                           "%s
Unable to compute required variables from provided `new_data`.
Does your interpolation data include all variables required by the model?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  
  object$model$remove_data()
  object$model$stage <- NULL
  
  check_transformation_data(object, new_data)
  trans <- map(object$transformation, bind_transformation_data, new_data)
  resp <- map2(seq_along(object$response), object$response, function(i, resp){
    expr(trans[[!!i]](!!resp))
  }) %>% 
    set_names(map_chr(object$response, as_string))
  
  new_data <- transmute(new_data, !!!resp)
  new_data <- interpolate(object[["fit"]], new_data = new_data, specials = specials, ...)
  new_data[names(resp)] <- map2(new_data[names(resp)], trans,
                                function(x, f) invert_transformation(f)(x))
  new_data
}