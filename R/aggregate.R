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
    gd <- group_data(group_by(.data, !!sym(idx), !!!set_names(map(x, function(.) expr(agg_vec(!!sym(.)))), x)))
    agg_keys <- setdiff(kv, x)
    agg_cols <- rep(list(agg_vec(NA, aggregated = TRUE)), length(agg_keys))
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

#' Create an aggregation vector
#' 
#' \lifecycle{maturing}
#' 
#' An aggregation vector extends usual vectors by adding <aggregated> values.
#' These vectors are typically produced via the [`aggregate_key()`] function,
#' however it can be useful to create them manually to produce more complicated
#' hierarchies (such as unbalanced hierarchies).
#' 
#' @param x The vector of values.
#' @param aggregated A logical vector to identify which values are `<aggregated>`.
#' 
#' @examples
#' agg_vec(
#'   x = c(NA, "A", "B"),
#'   aggregated = c(TRUE, FALSE, FALSE)
#' )
#' 
#' @export
agg_vec <- function(x = character(), aggregated = logical(vec_size(x))){
  is_agg <- is_aggregated(x)
  if (inherits(x, "agg_vec")) x <- field(x, "x")
  x[is_agg] <- NA
  vec_assert(aggregated, ptype = logical())
  vctrs::new_rcrd(list(x = x, agg = is_agg | aggregated), class = "agg_vec")
}

#' @export
format.agg_vec <- function(x, ..., na_chr = "<aggregated>"){
  n <- vec_size(x)
  x <- vec_data(x)
  is_agg <- x[["agg"]]
  out <- character(length = n)
  out[is_agg] <- na_chr
  out[!is_agg] <- format(x[["x"]][!is_agg], ...)
  out 
}

pillar_shaft.agg_vec <- function(x, ...) {
  if(requireNamespace("crayon")){
    na_chr <- crayon::style("<aggregated>", crayon::make_style("#999999", grey = TRUE))
  }
  else{
    na_chr <- "<aggregated>"
  }
  
  out <- format(x, na_chr = na_chr)
  
  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10)
}

#' Internal vctrs methods
#'
#' These methods are the extensions that allow aggregation vectors to work with
#' vctrs.
#'
#' @keywords internal
#' @name aggregation-vctrs
NULL

#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec <- function(x, y, ...) UseMethod("vec_ptype2.agg_vec", y)
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.agg_vec <- function(x, y, ...) {
  x <- vec_data(x)[["x"]]
  y <- vec_data(y)[["x"]]
  ptype <- if(!is_logical(x) && !is_logical(y)) {
    vec_ptype2(x, y)
  } else if (is_logical(x)) {
    y
  } else {
    x
  }
  agg_vec(ptype)
}
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.default <- function(x, y, ...) agg_vec()
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.agg_vec.character <- function(x, y, ...) agg_vec()
#' @rdname aggregation-vctrs
#' @export
vec_ptype2.character.agg_vec <- function(x, y, ...) agg_vec()

#' @rdname aggregation-vctrs
#' @export
vec_ptype_abbr.agg_vec <- function(x, ...) {
  paste0(vctrs::vec_ptype_abbr(vec_data(x)[["x"]], ...), "*")
}

#' @rdname aggregation-vctrs
#' @export
vec_cast.agg_vec <- function(x, to, ...) UseMethod("vec_cast.agg_vec")
#' @rdname aggregation-vctrs
#' @export
vec_cast.agg_vec.agg_vec <- function(x, to, ...) {
  x <- vec_proxy(x)
  if(all(x$agg)) x$x <- vec_rep(vec_cast(NA, vec_proxy(to)$x), length(x$x))
  vec_restore(x, to)
}
#' @rdname aggregation-vctrs
#' @export
vec_cast.agg_vec.default <- function(x, to, ...) agg_vec(x)
#' @export
vec_cast.agg_vec.character <- function(x, to, ...) agg_vec(x)
#' @rdname aggregation-vctrs
#' @export
vec_cast.character.agg_vec <- function(x, to, ...) trimws(format(x))

#' @rdname aggregation-vctrs
#' @export
vec_proxy_compare.agg_vec <- function(x, ...) {
  vec_proxy(x)[c(2,1)]
}

#' @export
`==.agg_vec` <- function(e1, e2){
  e1_agg <- inherits(e1, "agg_vec")
  e2_agg <- inherits(e2, "agg_vec")
  
  if(!e1_agg || !e2_agg){
    x <- list(e1,e2)[[which(!c(e1_agg, e2_agg))]]
    is_agg <- x == "<aggregated>"
    if(any(is_agg)){
      warn("<aggregated> character values have been converted to aggregated values.
Hint: If you're trying to compare aggregated values, use `is_aggregated()`.")
    }
    x <- agg_vec(ifelse(is_agg, NA, x), aggregated = is_agg)
    if(!e1_agg) e1 <- x else e2 <- x
  }
  
  x <- vec_recycle_common(e1, e2)
  e1 <- vec_proxy(x[[1]])
  e2 <- vec_proxy(x[[2]])
  out <- logical(vec_size(e1))
  (e1$agg & e2$agg) | vec_equal(e1$x, e2$x, na_equal = TRUE)
}

#' @export
`!=.agg_vec` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
is.na.agg_vec <- function(x) {
  is.na(field(x, "x")) & !field(x, "agg")
}

#' @importFrom dplyr recode
#' @export
recode.agg_vec <- function(.x, ...) {
  field(.x, "x") <- recode(field(.x, "x"), ...)
  .x
}

#' Is the element an aggregation of smaller data
#' 
#' @param x An object.
#' 
#' @seealso [`aggregate_key`]
#' 
#' @export
is_aggregated <- function(x){
  if(!inherits(x, "agg_vec")){
    logical(vec_size(x))
  } else {
    vec_proxy(x)[["agg"]]
  }
}

scale_type.agg_vec <- function(x) {
  abort("Cannot add an aggregated vector to a plot, use format() to plot with your aggregations.")
}
# # Space efficient group identifiers, storing group positions as vectors instead of lists
# group_by_alt <- function(.data, ...){
#   .data <- transmute(as_tibble(.data), ...)
#   grps <- vctrs::vec_group_id(.data)
#   list(pos = order(grps, method = "radix"), len = tabulate(grps), var = names(.data))
# }
# 
# # Simplified summarise(), which uses the group identifiers from group_by_alt()
# summarise_alt <- function(.data, ..., .grps = group_by_alt(.data)){
#   .data <- as.data.frame(.data)
#   dots <- enquos(..., .named = TRUE)
#   dots_names <- names(dots)
#   
#   grp_pos <- .grps[["pos"]]
#   grp_len <- .grps[["len"]]
#   
#   n_grps <- length(grp_len)
#   grp_start <- 1 + cumsum(c(0, grp_len[-n_grps]))
#   out <- vec_slice(.data[.grps[["var"]]], grp_pos[grp_start])
#   
#   # Promise based group aware data mask (adapted from dplyr:::DataMask)
#   bindings <- env(empty_env())
#   resolved <- rep_len(TRUE, ncol(.data))
#   promise_fn <- function(index) {
#     resolved[[index]] <<- TRUE
#     vec_slice(.subset2(.data, index), rows)
#   }
#   promise_env <- get_env(promise_fn)
#   nm <- names2(.data)
#   promises <- map(seq_len(ncol(.data)), function(.x) expr(delayedAssign(!!nm[[.x]], promise_fn(!!.x), env, bindings)))
#   env <- current_env()
#   # env_bind_lazy(bindings, !!!promises)
#   mask <- new_data_mask(bindings)
#   mask$.data <- as_data_pronoun(mask)
#   
#   # Compute dots over groups
#   for(i in seq_along(dots)){
#     res <- NULL
#     dot <- dots[[i]]
#     for(grp in seq_len(n_grps)){
#       # Set groups and reset resolved promises
#       size <- grp_len[grp]
#       idx <- grp_start[grp]
#       promise_env$rows <- .subset(grp_pos, idx:(idx+size-1))
#       for(j in which(resolved)){
#         eval(promises[[j]])
#         resolved[j] <- FALSE
#       }
#       
#       # Compute and store calculation
#       val <- eval_tidy(dot, mask)
#       if(is.null(res)){
#         res <- vec_init(val, n = n_grps)
#       }
#       res[grp] <- val
#     }
#     out[names(dots)[[i]]] <- res
#   }
#   out
# }