#' Model based decomposition
#' 
#' @inheritParams estimate
#' 
#' @rdname decompose
#' 
#' @export
decompose <- function(.data, ...){
  UseMethod("decompose")
}

#' @rdname decompose
#' @export
decompose.tbl_ts <- function(.data, .model, ...){
  if(!inherits(.model, "mdl_defn")){
    abort("Model definition incorrectly created. Check that specified model(s) are model definitions.")
  }
  
  components(model(.data, .model))[-1]
}
