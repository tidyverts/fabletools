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
  if(NROW(object) > 1 || length(object%@%"models") > 1){
    glance(object)
  }
  else{
    report(object[[as_string((object%@%"models")[[1]])]][[1]])
  }
}

#' @export
report.model <- function(object, ...){
  cat(paste("Series:", expr_text(object$response), "\n"))
  cat(paste("Model:", model_sum(object), "\n"))
  if(!is_symbol(body(object$transformation))){
    cat(paste("Transformation:", expr_text(body(object$transformation)), "\n"))
  }
  report(object[["fit"]])
}