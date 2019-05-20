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
    if(!is_call(spec)){
      return(as_string(spec))
    }
    eval_tidy(spec, env = env(
      `*` = function(e1, e2) {
        e1 <- parse_spec(enexpr(e1))
        e2 <- parse_spec(enexpr(e2))
        c(
          e1, e2,
          flatten(map(e1, function(x){
            map(e2, function(y){
              c(x, y)
            })
          }))
        )
      },
      `/` = function(e1, e2) {
        e1 <- enexpr(e1)
        e2 <- enexpr(e2)
        if(is_call(e1)) abort("Hierarchical structure must be specified for specific nodes. Try adding more parenthesis.")
        e1 <- parse_spec(e1)
        e2 <- parse_spec(e2)
        c(
          e1,
          map(e2, function(x, y) c(y, x), e1)
        )
      }
    ))
  }
  
  # Key combinations
  key_comb <- c(list(chr()), parse_spec(structure))
  
  idx <- index2(data)
  data <- as_tibble(data)
  
  bind_row_attrb <- function(x){
    attrb <- transpose(map(x, function(dt) map(dt, attributes)))
    simple_attrb <- map_lgl(attrb, function(x) length(unique(x)) == 1)
    
    x <- dplyr::bind_rows(!!!x)
    
    for (col in which(simple_attrb)){
      attributes(x[[col]]) <- attrb[[col]][[1]]
    }
    x
  }
  
  agg_dt <- bind_row_attrb(map(key_comb, function(x){
    group_data(group_by(data, !!idx, !!!syms(x)))
  }))
  
  kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
  agg_dt <- agg_dt[c(as_string(idx), kv, ".rows")]
  
  data <- new_tibble(data, groups = agg_dt, subclass = "grouped_df")
  
  # # Aggregate variables
  # agg_dt$.rows <- map(agg_dt$.rows, function(comb_rows){
  #   as_tsibble(c(
  #     set_names(list(idx[comb_rows[[1]]]), idx_chr),
  #     map(vars, function(x){
  #       possibly(reduce, rep(NA, length(comb_rows[[1]])))(
  #         comb_rows[-1], 
  #         function(tot, y) tot + x[y], 
  #         .init = x[comb_rows[[1]]]
  #       )
  #     })
  #   ), index = idx_chr)
  # })
  
  # Compute aggregates
  data <- ungroup(summarise(data, ...))
  
  # Return tsibble
  as_tsibble(data, key = kv, index = !!idx)
}
