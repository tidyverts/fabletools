#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables(".")

#' @import rlang
#' @import tsibble
#' @importFrom dplyr mutate transmute summarise filter select rename group_by ungroup groups group_data anti_join left_join semi_join
#' @importFrom dplyr dplyr_row_slice dplyr_col_modify dplyr_reconstruct
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr nest unnest gather spread
#' @importFrom vctrs vec_data vec_assert vec_size vec_is df_ptype2 list_of tib_cast
NULL