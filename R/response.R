#' Extract the respone data from a model
#' 
#' @param object The object containing response data
#' @param ... Additional parameters passed on to other methods
#' 
#' @export
response <- function(object, ...){
  UseMethod("response")
}

#' @export
response.mdl_df <- function(object, ...){
  out <- gather(object, ".model", ".fit", !!!(object%@%"models"))
  keys <- key(out)
  out <- transmute(out,
                   !!!keys,
                   !!sym(".model"),
                   response = map(!!sym(".fit"), response)
  )
  unnest(add_class(out, "lst_ts"), key = keys)
}

#' @export
response.model <- function(object, ...){
  aug <- augment(object, ...)
  select(aug, !!index(aug), !!!list(.response = object$response))
}