#' Expand a dataset to include all levels of aggregation
#' 
#' All keys must be of the same length over the same time period, and a grouping
#' aggregation structure will be used.
#' 
#' @param data A dataset to aggregate.
#' @param structure The specification of aggregation structure.
#' @param ... Further arguments passed to other methods.
#' 
#' @examples 
#' library(tsibble)
#' aggregate_keys(tourism)
#' 
#' @export
aggregate_keys <- function(data, structure, ...){
  UseMethod("aggregate_keys")
}

#' @export
aggregate_keys.tbl_ts <- function(data, structure = NULL, ...){
  message("Note: reconciliation in fable is highly experimental. The interface will be refined in the near future.")
  
  structure <- enexpr(structure)
  if(is.null(structure)){
    message(
      sprintf("Key structural specification not found, defaulting to `structure = %s`",
              paste(key_vars(data), collapse = "*"))
    )
    structure <- parse_expr(paste(key_vars(data), collapse = "*"))
  }
  
  parse_spec <- function(spec){
    if(!is_call(spec)) return(spec)
    eval_tidy(spec, env = env(
      `*` = function(e1, e2) {
        list(parse_spec(enexpr(e1)), parse_spec(enexpr(e2)))
      },
      `/` = function(e1, e2) {
        
      }
    ))
  }
  
  parse_spec(spec)
  
  browser()
  
  # Extract data
  kv <- all.vars(spec)
  vars <- as.list(data[measured_vars(data)])
  idx_chr <- as_string(index(data))
  idx <- data[[idx_chr]]
  
  # Key combinations
  key_comb <- flatten(map(c(0, seq_along(kv)), combn, x = kv, simplify = FALSE))
  key_dt <- key_data(data)
  agg_dt <- invoke(dplyr::bind_rows, map(key_comb, function(x){
    summarise(group_by(key_dt, !!!syms(x)), .rows = list(!!sym(".rows")))
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
  unnest(add_class(agg_dt, "lst_ts"), !!sym(".rows"), key = kv)
}
