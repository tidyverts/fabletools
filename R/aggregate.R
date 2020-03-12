#' Expand a dataset to include other levels of aggregation
#' 
#' Uses the structural specification given in `.spec` to aggregate a time
#' series. A grouped structure is specified using `grp1 * grp2`, and a nested 
#' structure is specified via `parent / child`. Aggregating the key structure is
#' commonly used with forecast reconciliation to produce coherent forecasts over
#' some hierarchy.
#' 
#' This function is experimental, and is subject to change in the future.
#' 
#' The way in which the measured variables are aggregated is specified in a
#' similar way to how `[dplyr::summarise()]` is used.
#' 
#' @param .data A tsibble.
#' @param .spec The specification of aggregation structure.
#' @inheritParams dplyr::summarise
#' 
#' @seealso 
#' [`reconcile()`], [`is_aggregated()`]
#' 
#' @examples 
#' library(tsibble)
#' tourism %>% 
#'   aggregate_key(Purpose * (State / Region), Trips = sum(Trips))
#' 
#' @export
aggregate_key <- function(.data, .spec, ...){
  UseMethod("aggregate_key")
}

#' @export
aggregate_key.tbl_ts <- function(.data, .spec = NULL, ...){
  .spec <- enexpr(.spec)
  if(is.null(.spec)){
    message(
      sprintf("Key structural specification not found, defaulting to `.spec = %s`",
              paste(key_vars(.data), collapse = "*"))
    )
    .spec <- parse_expr(paste(key_vars(.data), collapse = "*"))
  }
  
  # Key combinations
  tm <- stats::terms(new_formula(lhs = NULL, rhs = .spec), env = empty_env())
  key_comb <- attr(tm, "factors")
  key_vars <- rownames(key_comb)
  key_comb <- map(split(key_comb, col(key_comb)), function(x) key_vars[x!=0])
  
  if(attr(tm, "intercept")){
    key_comb <- c(list(chr()), key_comb)
  }
  
  idx <- index2(.data)
  intvl <- interval(.data)
  .data <- as_tibble(.data)
  
  agg_dt <- bind_row_attrb(map(key_comb, function(x){
    group_data(group_by(.data, !!idx, !!!syms(x)))
  }))
  
  kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
  agg_dt <- agg_dt[c(kv, as_string(idx), ".rows")]
  
  .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
  
  # Compute aggregates
  .data <- summarise(.data, ...)
  key_dt <- group_data(group_by(.data, !!!syms(kv)))
  .data <- ungroup(.data)
  
  # Return tsibble
  build_tsibble_meta(.data, key_data = key_dt, index = as_string(idx), 
                     index2 = as_string(idx), ordered = TRUE,
                     interval = intvl) %>% 
    mutate(!!!set_names(map(kv, function(x) expr(agg_key(!!sym(x)))), kv))
}


# #' @rdname aggregate_key
# #' 
# #' @param .times Temporal aggregations to include. The default (NULL) will
# #' automatically identify appropriate temporal aggregations. This can be specified
# #' as a list of [`lubridate::period()`] elements, or a character vector describing the
# #' temporal aggregations.
# #' 
# #' @examples
# #' library(tsibble)
# #' pedestrian %>% 
# #'   aggregate_index()
aggregate_index <- function(.data, .times, ...){
  UseMethod("aggregate_index")
}

#' @export
aggregate_index.tbl_ts <- function(.data, .times = NULL, ...){
  warn("Temporal aggregation is highly experimental. The interface will be refined in the near future.")
  
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
  agg_dt <- vctrs::vec_rbind(
    !!!map(seq_along(.times), function(tm){
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
  
  # Compute aggregates and repair index attributes
  idx_attr <- attributes(.data[[as_string(idx)]])
  .data <- ungroup(summarise(.data, ...))
  attributes(.data[[as_string(idx)]]) <- idx_attr
  
  # Return tsibble
  as_tsibble(.data, key = kv, index = !!idx) %>% 
    mutate(!!!set_names(map(kv, function(x) expr(agg_key(!!sym(x)))), kv))
}

agg_key <- function(x){
  add_class(x, "agg_key")
}

#' @export
print.agg_key <- function(x, ...){
  print(trimws(format(x, ...)))
}

#' @export
format.agg_key <- function(x, ..., na_chr = "<aggregated>"){
  na_pos <- is.na(x)
  out <- NextMethod(na.encode = FALSE)
  out[na_pos] <- na_chr
  out 
}

#' @export
`[.agg_key` <- function(...){
  agg_key(NextMethod())
}

#' @export
unique.agg_key <- function(x, incomparables = FALSE, ...){
  agg_key(NextMethod())
}

pillar_shaft.agg_key <- function(x, ...) {
  if(requireNamespace("crayon")){
    na_chr <- crayon::style("<aggregated>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    na_chr <- "<aggregated>"
  }
  
  out <- format(x, na_chr = na_chr)
  
  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10)
}

type_sum.agg_key <- function(x){
  pillar::type_sum(rm_class(x, "agg_key"))
}

obj_sum.agg_key <- function(x){
  pillar::obj_sum(rm_class(x, "agg_key"))
}

#' Is the element an aggregation of smaller data
#' 
#' @param x An object.
#' 
#' @seealso [`aggregate_key`]
#' 
#' @export
is_aggregated <- function(x){
  if(inherits(x, "agg_key")){
    is.na(x)
  }
  else{
    rep(FALSE, length(x))
  }
}