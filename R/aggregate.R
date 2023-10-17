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
aggregate_key.tbl_ts <- function(.data, .spec = NULL, ...){#, dev = FALSE){
  .spec <- enexpr(.spec)
  if(is.null(.spec)){
    kv <- syms(key_vars(.data))
    message(
      sprintf("Key structural specification not found, defaulting to `.spec = %s`",
              paste(kv, collapse = "*"))
    )
    .spec <- reduce(kv, call2, .fn = "*")
  }

  key_comb <- parse_agg_spec(.spec)
  
  idx <- index2_var(.data)
  intvl <- interval(.data)
  kd <- key_data(.data)
  cn <- colnames(.data)
  has_varied_index <- any(has_gaps(.data, .full = TRUE)[[".gaps"]]) && !is_ordered(.data)
  .data <- as_tibble(.data)
  
  kv <- unique(unlist(key_comb, recursive = FALSE))
  # if(dev){
  #   .data <- map(unname(key_comb), function(x){
  #     agg <- summarise_alt(.data, ..., 
  #                          .grps = group_by_alt(.data, !!sym(idx), !!!syms(x)))
  #     agg[x] <- map(agg[x], agg_vec)
  #     agg_keys <- setdiff(kv, x)
  #     agg[agg_keys] <- rep(list(agg_vec(NA_character_, aggregated = TRUE)), length(agg_keys))
  #     agg
  #   })
  #   .data <- vctrs::vec_rbind(!!!.data)
  # } else {
  agg_dt <- map(unname(key_comb), function(x){
    gd <- group_data(group_by(.data, !!sym(idx), !!!set_names(map(x, function(.) expr(graphvec::agg_vec(!!sym(.)))), x)))
    agg_keys <- setdiff(kv, x)
    agg_cols <- rep(list(graphvec::agg_vec(NA, aggregated = TRUE)), length(agg_keys))
    gd[agg_keys] <- agg_cols
    gd[c(idx, kv, ".rows")]
  })
  agg_dt <- vctrs::vec_rbind(!!!agg_dt)
  .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
  .data <- summarise(.data, ...)
  # }
  
  # Re-order columns into index, keys, values order
  .data <- .data[c(idx, kv, setdiff(colnames(.data), c(idx,kv)))]
  
  key_dt <- group_data(group_by(.data, !!!syms(kv)))
  .data <- ungroup(.data)
  
  # Return tsibble
  build_tsibble_meta(.data, key_data = key_dt, index = idx, 
                     index2 = as_string(idx), ordered = TRUE,
                     interval = intvl)
}

parse_agg_spec <- function(expr){
  # Key combinations
  tm <- stats::terms(new_formula(lhs = NULL, rhs = expr), env = empty_env())
  key_comb <- attr(tm, "factors")
  key_vars <- sub("^`(.*)`$", "\\1", rownames(key_comb))
  key_comb <- map(split(key_comb, col(key_comb)), function(x) key_vars[x!=0])
  if(attr(tm, "intercept")){
    key_comb <- c(list(chr()), key_comb)
  }
  unname(key_comb)
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
# aggregate_index <- function(.data, .times, ...){
#   UseMethod("aggregate_index")
# }
# 
# #' @export
# aggregate_index.tbl_ts <- function(.data, .times = NULL, ...){
#   warn("Temporal aggregation is highly experimental. The interface will be refined in the near future.")
#   
#   require_package("lubridate")
#   idx <- index(.data)
#   kv <- key_vars(.data)
#   
#   # Parse times as lubridate::period
#   if(is.null(.times)){
#     interval <- with(interval(.data), lubridate::years(year) + 
#            lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
#            lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) + 
#            lubridate::seconds(second) + lubridate::milliseconds(millisecond) + 
#            lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
#     periods <- common_periods(.data)
#     .times <- c(set_names(names(periods), names(periods)), list2(!!format(interval(.data)) := interval))
#   }
#   .times <- set_names(map(.times, lubridate::as.period), names(.times) %||% .times)
#   
#   secs <- map_dbl(.times, lubridate::period_to_seconds)
#   .times <- .times[order(secs, decreasing = TRUE)]
#   
#   # Temporal aggregations
#   .data <- as_tibble(.data)
#   agg_dt <- vctrs::vec_rbind(
#     !!!map(seq_along(.times), function(tm){
#       group_data(
#         group_by(.data,
#                  !!!set_names(names(.times), names(.times))[seq_len(tm-1) + 1],
#                  !!as_string(idx) := lubridate::floor_date(!!idx, .times[[tm]]),
#                  !!!syms(kv))
#       )
#     })
#   )
#   kv <- setdiff(colnames(agg_dt), c(as_string(idx), ".rows"))
#   agg_dt <- agg_dt[c(as_string(idx), kv, ".rows")]
#   
#   .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
#   
#   # Compute aggregates and repair index attributes
#   idx_attr <- attributes(.data[[as_string(idx)]])
#   .data <- ungroup(summarise(.data, ...))
#   attributes(.data[[as_string(idx)]]) <- idx_attr
#   
#   # Return tsibble
#   as_tsibble(.data, key = kv, index = !!idx) %>% 
#     mutate(!!!set_names(map(kv, function(x) expr(agg_vec(!!sym(x)))), kv))
# }

scale_type.agg_vec <- function(x) {
  abort("Cannot add an aggregated vector to a plot, use format() to plot with your aggregations.")
}