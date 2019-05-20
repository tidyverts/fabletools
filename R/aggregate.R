parse_aggregation <- function(spec){
  if(!is_call(spec)){
    return(as_string(spec))
  }
  eval_tidy(spec, env = env(
    `*` = function(e1, e2) {
      e1 <- parse_aggregation(enexpr(e1))
      e2 <- parse_aggregation(enexpr(e2))
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
      e1 <- parse_aggregation(e1)
      e2 <- parse_aggregation(e2)
      c(
        e1,
        map(e2, function(x, y) c(y, x), e1)
      )
    }
  ))
}

#' Expand a dataset to include other levels of aggregation
#' 
#' Uses the structural specification given in `.spec` to aggregate a time
#' series. Commonly used in combination with forecast reconciliation.
#' 
#' A grouped structure is specified using `grp1 * grp2`, and a nested structure 
#' is specified via `parent / child`. 
#' 
#' @param .data A tsibble.
#' @inheritParams dplyr::summarise
#' @param .spec The specification of aggregation structure.
#' 
#' @examples 
#' library(tsibble)
#' tourism_grp <- tourism %>% 
#'   aggregate_keys(Purpose * (State / Region), Trips = sum(Trips))
#' 
#' @export
aggregate_keys <- function(.data, .spec, ...){
  UseMethod("aggregate_keys")
}

#' @export
aggregate_keys.tbl_ts <- function(.data, .spec = NULL, ...){
  .spec <- enexpr(.spec)
  if(is.null(.spec)){
    message(
      sprintf("Key structural specification not found, defaulting to `structure = %s`",
              paste(key_vars(.data), collapse = "*"))
    )
    .spec <- parse_expr(paste(key_vars(.data), collapse = "*"))
  }
  
  # Key combinations
  key_comb <- c(list(chr()), parse_aggregation(.spec))
  
  idx <- index2(.data)
  .data <- as_tibble(.data)
  
  agg_dt <- bind_row_attrb(map(key_comb, function(x){
    group_data(group_by(.data, !!idx, !!!syms(x)))
  }))
  
  kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
  agg_dt <- agg_dt[c(as_string(idx), kv, ".rows")]
  
  .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
  
  # Compute aggregates
  .data <- ungroup(summarise(.data, ...))
  
  # Return tsibble
  as_tsibble(.data, key = kv, index = !!idx) %>% 
    mutate(!!!set_names(map(kv, function(x) expr(agg_key(!!sym(x)))), kv))
}


#' @rdname aggregate_keys
#' 
#' @param .times Temporal aggregations to include. The default (NULL) will
#' automatically identify appropriate temporal aggregations. This can be specified
#' as a list of [`lubridate::period()`] elements, or a character vector describing the
#' temporal aggregations.
#' 
#' @examples
#' library(tsibble)
#' pedestrian %>% 
#'   aggregate_index()
#' 
#' @export
aggregate_index <- function(.data, .times, ...){
  UseMethod("aggregate_index")
}

#' @export
aggregate_index.tbl_ts <- function(.data, .times = NULL, ...){
  message("Note: temporal aggregation is highly experimental. The interface will be refined in the near future.")
  
  require_package("lubridate")
  idx <- index(.data)
  kv <- key_vars(.data)
  
  # Parse times as lubridate::period
  if(is.null(.times)){
    interval <- with(interval(.data), lubridate::years(year) + 
           lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
           lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) + 
           lubridate::seconds(second) + lubridate::milliseconds(millisecond) + 
           lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
    periods <- common_periods(.data)
    .times <- c(set_names(names(periods), names(periods)), list2(!!format(interval(.data)) := interval))
  }
  .times <- set_names(map(.times, lubridate::as.period), names(.times) %||% .times)
  
  secs <- map_dbl(.times, lubridate::period_to_seconds)
  .times <- .times[order(secs, decreasing = TRUE)]
  
  # Temporal aggregations
  .data <- as_tibble(.data)
  agg_dt <- invoke(dplyr::bind_rows,
    map(seq_along(.times), function(tm){
      group_data(
        group_by(.data,
                 !!!set_names(names(.times), names(.times))[seq_len(tm-1) + 1],
                 !!as_string(idx) := lubridate::floor_date(!!idx, .times[[tm]]),
                 !!!syms(kv))
      )
    })
  )
  kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
  agg_dt <- agg_dt[c(as_string(idx), kv, ".rows")]
  
  .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
  
  # Compute aggregates
  .data <- ungroup(summarise(.data, ...))
  
  # Return tsibble
  as_tsibble(.data, key = kv, index = !!idx) %>% 
    mutate(!!!set_names(map(kv, function(x) expr(agg_key(!!sym(x)))), kv))
}

agg_key <- function(x){
  add_class(x, "agg_key")
}

#' @export
print.agg_key <- function(x){
  print(trimws(format(x)))
}

#' @export
format.agg_key <- function(x, na_chr = "<total>"){
  out <- NextMethod(na.encode = FALSE)
  out[is.na(out)] <- na_chr
  out 
}

#' @export
`[.agg_key` <- function(...){
  agg_key(NextMethod())
}

pillar_shaft.agg_key <- function(x, ...) {
  if(requireNamespace("crayon")){
    na_chr <- crayon::style("<total>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    na_chr <- "<total>"
  }
  
  out <- format(x, na_chr = na_chr)
  
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 10)
}

type_sum.agg_key <- function(x){
  pillar::type_sum(rm_class(x, "agg_key"))
}

obj_sum.agg_key <- function(x){
  pillar::obj_sum(rm_class(x, "agg_key"))
}