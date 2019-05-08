#' Expand a dataset to include all levels of aggregation
#' 
#' All keys must be of the same length over the same time period, and a grouping
#' aggregation structure will be used.
#' 
#' @param data A dataset to aggregate.
#' @param ... Further arguments passed to other methods.
#' 
#' @examples 
#' library(tsibble)
#' expand_keys(tourism)
#' 
#' @export
expand_keys <- function(data, ...){
  UseMethod("expand_keys")
}

#' @export
expand_keys.tbl_ts <- function(data, ...){
  message("Note: reconciliation in fable is highly experimental. The interface will be refined in the near future.")
  # Extract data
  kv <- key_vars(data)
  vars <- as.list(data[measured_vars(data)])
  idx_chr <- as_string(index(data))
  idx <- data[[idx_chr]]
  
  # Key combinations
  key_comb <- flatten(map(c(0, seq_along(kv)), combn, x = kv, simplify = FALSE))
  key_dt <- key_data(data)
  agg_dt <- invoke(dplyr::bind_rows, map(key_comb, function(x){
    summarise(group_by(key_dt, !!!syms(x)), .rows = list(.rows))
  }))
  
  # Aggregate variables
  agg_dt$.rows <- map(agg_dt$.rows, function(comb_rows){
    as_tsibble(c(
      set_names(list(idx[comb_rows[[1]]]), idx_chr),
      map(vars, function(x){
        possibly(reduce, rep(NA, length(comb_rows[[1]])))(
          comb_rows[-1], 
          function(tot, y) tot + x[y], 
          .init = x[comb_rows[[1]]]
        )
      })
    ), index = idx_chr)
  })
  
  # Return tsibble
  unnest(add_class(agg_dt, "lst_ts"), .rows, key = kv)
}
}
