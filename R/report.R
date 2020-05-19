#' Report information about an object
#' 
#' Displays the object in a suitable format for reporting.
#' 
#' @param object The object to report
#' @param ... Additional options for the reporting function
#' 
#' @export
report <- function(object, ...){
  UseMethod("report")
}

#' @export
report.mdl_df <- function(object, ...){
  if(NROW(object) > 1 || length(mable_vars(object)) > 1){
    warning("Model reporting is only supported for individual models, so a glance will be shown. To see the report for a specific model, use `select()` and `filter()` to identify a single model.")
    return(glance(object))
  }
  else{
    report(object[[mable_vars(object)[[1]]]][[1]])
  }
  invisible(object)
}

#' @export
report.mdl_ts <- function(object, ...){
  cat(paste("Series:", paste0(map(object$response, expr_name), collapse = ", "), "\n"))
  cat(paste("Model:", model_sum(object), "\n"))
  if(!is_symbol(body(object$transformation[[1]])) && length(object$response) == 1){
    cat(paste("Transformation:", expr_name(body(object$transformation[[1]])), "\n"))
  }
  tryCatch(
    report(object[["fit"]]),
    error = function(e){
      cat("\nA model specific report is not available for this model class.")
    }
  )
}