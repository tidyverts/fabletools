#' @importFrom utils head capture.output
#' @export
summary.mdl_df <- function(object, ...){
  map(head(object[[(object%@%"models")[[1]]]]), function(.x) capture.output(summary(.x))) %>%
    invoke(cat, ., sep="\n")
  invisible(object)
}

#' @export
summary.model <- function(object, ...){
  summary(object$fit, ...)
}

#' @export
summary.fbl_ts <- function(object, level=c(80,95), ...){
  .Deprecated("Please use report()")
  object %>%
    transmute(
      !!(object%@%"response"),
      !!!set_names(map(level,function(.x) expr(hilo(!!(object%@%"dist"), !!.x))),
                   paste0(level, "%")))
}