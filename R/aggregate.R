#' Expand a dataset to include other levels of aggregation
#' 
#' Uses the structural specification given in `structure` to aggregate a time
#' series. Commonly used in combination with forecast reconciliation.
#' 
#' A grouped structure is specified using `grp1 * grp2`, and a nested structure 
#' is specified via `parent / child`. 
#' 
#' @inheritParams dplyr::summarise
#' @param structure The specification of aggregation structure.
#' 
#' @examples 
#' library(tsibble)
#' tourism_grp <- tourism %>% 
#'   aggregate_keys(Purpose * (State / Region), Trips = sum(Trips))
#' 
#' @export
aggregate_keys <- function(.data, structure, ...){
  UseMethod("aggregate_keys")
}

#' @export
aggregate_keys.tbl_ts <- function(.data, structure = NULL, ...){
  message("Note: reconciliation in fable is highly experimental. The interface will be refined in the near future.")
  
  structure <- enexpr(structure)
  if(is.null(structure)){
    message(
      sprintf("Key structural specification not found, defaulting to `structure = %s`",
              paste(key_vars(.data), collapse = "*"))
    )
    structure <- parse_expr(paste(key_vars(.data), collapse = "*"))
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
  
  idx <- index2(.data)
  .data <- as_tibble(.data)
  
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
    group_data(group_by(.data, !!idx, !!!syms(x)))
  }))
  
  kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
  agg_dt <- agg_dt[c(as_string(idx), kv, ".rows")]
  
  .data <- new_tibble(.data, groups = agg_dt, subclass = "grouped_df")
  
  # Compute aggregates
  .data <- ungroup(summarise(.data, ...))
  
  # Return tsibble
  as_tsibble(.data, key = kv, index = !!idx)
}
