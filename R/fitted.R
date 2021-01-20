#' Extract fitted values from models
#' 
#' Extracts the fitted values from each of the models in a mable. A tsibble will
#' be returned containing these fitted values. Fitted values will be 
#' automatically back-transformed if a transformation was specified.
#' 
#' @aliases  hfitted
#' 
#' @param object A mable or time series model.
#' @param ... Other arguments passed to the model method for `fitted()`
#' 
#' @importFrom stats fitted
#' @export
fitted.mdl_df <- function(object, ...){
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  object <- mutate(as_tibble(object), 
              dplyr::across(all_of(mbl_vars), function(x) lapply(x, fitted, ...)))
  object <- pivot_longer(object, mbl_vars, names_to = ".model", values_to = ".fitted")
  unnest_tsbl(object, ".fitted", parent_key = c(kv, ".model"))
}

#' @rdname fitted.mdl_df
#' 
#' @param h The number of steps ahead that these fitted values are computed from.
#' 
#' @export
fitted.mdl_ts <- function(object, h = 1, ...){
  bt <- map(object$transformation, invert_transformation)
  
  fits <- if(h==1) fitted(object$fit, ...) else hfitted(object, h = h, ...)
  if(h == 1){
    fits <- as.matrix(fits)
    # Backtransformation is required for fitted, but forecast() handles it already.
    fits <- map2(bt, split(fits, col(fits)), function(bt, fit) bt(fit))
  }
  
  nm <- if(length(fits) == 1) ".fitted" else map_chr(object$response, expr_name)
  
  out <- object$data[index_var(object$data)]
  out[nm] <- fits
  out
}

#' @export
hfitted <- function(object, ...) {
  UseMethod("hfitted")
}

#' @export
hfitted.mdl_ts <- function(object, h, ...) {
  fn <- tryCatch(utils::getS3method("hfitted", class(object[["fit"]])),
                 error = function(e) NULL)
  if(is.null(fn)) {
    dt <- object$data
    resp <- response_vars(object)
    
    # Undo transformations
    bt <- lapply(object$transformation, invert_transformation)
    mv <- match(measured_vars(dt), names(dt))
    dt[mv] <- mapply(calc, bt, dt[measured_vars(dt)], SIMPLIFY = FALSE)
    names(dt)[mv] <- resp
    
    n <- nrow(dt)
    fits <- rep(NA_real_, n)
    
    for (i in seq_len(n-h)) {
      mdl <- tryCatch(refit(object, vec_slice(dt, seq_len(i))),
                      error = function(e) NULL)
      if(is.null(mdl)) next
      fits[i + h] <- mean(forecast(mdl, h = h, point_forecast = NULL)[[resp]][h])
    }
    fits <- list(fits)
  } else {
    fits <- as.matrix(fn(object[["fit"]], h=h, ...))
    # Backtransform fits from model method
    bt <- map(object$transformation, invert_transformation)
    fits <- map2(bt, split(fits, col(fits)), function(bt, fit) bt(fit))
  }
  fits
}