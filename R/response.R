#' Extract the response data from a model
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
  kv <- key_vars(out)
  out <- transmute(as_tibble(out),
                   !!!syms(kv),
                   !!sym(".model"),
                   response = map(!!sym(".fit"), response)
  )
  unnest(add_class(out, "lst_ts"), key = kv)
}

#' @export
response.model <- function(object, ...){
  aug <- augment(object, ...)
  bt <- invert_transformation(object$transformation)
  transmute(aug, !!index(aug), 
            .response = bt(!!sym(expr_text(model_lhs(object$model))))
  )
}