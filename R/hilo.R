#' Unpack a hilo column
#' 
#' Allows a hilo column to be unpacked into its component columns: "lower", 
#' "upper", and "level".
#' 
#' @inheritParams tidyr::pack
#' @param cols Name of hilo columns to unpack.
#' 
#' @seealso [`tidyr::unpack()`]
#' 
#' @export
unpack_hilo <- function(data, cols, names_sep = NULL, names_repair = "check_unique"){
  idx <- index_var(data)
  idx2 <- index2_var(data)
  kd <- key_data(data)
  ordered <- is_ordered(data)
  intvl <- interval(data)
  
  cols <- tidyselect::vars_select(tbl_vars(data), !!enquo(cols))
  data[cols] <- map(cols, function(col) as_tibble(vctrs::vec_data(data[[col]])))
  data <- tidyr::unpack(data, cols, names_sep = names_sep, names_repair = names_repair)
  build_tsibble_meta(data, key_data = kd, index = idx, index2 = idx2,
                     ordered = ordered, interval = intvl)
}
