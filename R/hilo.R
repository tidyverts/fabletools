#' Unpack a hilo column
#'
#' @description 
#' `r lifecycle::badge('superseded')`
#' 
#' This function is superceded. It is recommended that you use the functionality
#' from the [distributional](https://pkg.mitchelloharawild.com/distributional/)
#' package to extract elements from a <hilo> object. For example, you can access
#' the lower bound with `<hilo>$lower`.
#' 
#' Allows a hilo column to be unpacked into its component columns: "lower", 
#' "upper", and "level".
#' 
#' @inheritParams tidyr::pack
#' @param cols Name of hilo columns to unpack.
#' 
#' @seealso [`tidyr::unpack()`]
#' 
#' @keywords internal
#' @export
unpack_hilo <- function(data, cols, names_sep = "_", names_repair = "check_unique"){
  orig <- data
  cols <- tidyselect::eval_select(enexpr(cols), data)
  if(any(bad_col <- !map_lgl(data[cols], inherits, "hilo"))){
    abort(sprintf(
      "Not all unpacking columns are hilo objects (%s). All unpacking columns of unpack_hilo() must be hilo vectors.",
      paste(names(bad_col)[bad_col], collapse = ", ")
    ))
  }
  data[cols] <- map(data[cols], function(x) vctrs::vec_proxy(x)[c("lower", "upper")])
  data <- tidyr::unpack(data, cols, names_sep = names_sep, names_repair = names_repair)
  vctrs::vec_restore(data, orig)
}
