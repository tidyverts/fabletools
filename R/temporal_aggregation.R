# Adapted from cut.Date
date_breaks <- function(x, breaks, start_monday = TRUE, offset = TRUE){
  # Currently only dates are supported
  x <- as.Date(x)
  by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
    stop("invalid specification of 'breaks'")
  valid <- pmatch(by2[length(by2)], c("days", "weeks", 
                                      "months", "years", "quarters"))
  if (is.na(valid)) 
    stop("invalid specification of 'breaks'")
  start <- as.POSIXlt(min(x, na.rm = TRUE))
  if (valid == 1L) 
    incr <- 1L
  if (valid == 2L) {
    start$mday <- start$mday - start$wday
    if (start_monday) 
      start$mday <- start$mday + ifelse(start$wday > 
                                          0L, 1L, -6L)
    start$isdst <- -1L
    incr <- 7L
  }
  if (valid == 3L) {
    start$mday <- 1L
    start$isdst <- -1L
    end <- as.POSIXlt(max(x, na.rm = TRUE))
    step <- if (length(by2) == 2L) 
      as.integer(by2[1L])
    else 1L
    end <- as.POSIXlt(end + (31 * step * 86400))
    end$mday <- 1L
    end$isdst <- -1L
    breaks <- as.Date(seq(start, end, breaks))
  }
  else if (valid == 4L) {
    start$mon <- 0L
    start$mday <- 1L
    start$isdst <- -1L
    end <- as.POSIXlt(max(x, na.rm = TRUE))
    step <- if (length(by2) == 2L) 
      as.integer(by2[1L])
    else 1L
    end <- as.POSIXlt(end + (366 * step * 86400))
    end$mon <- 0L
    end$mday <- 1L
    end$isdst <- -1L
    breaks <- as.Date(seq(start, end, breaks))
  }
  else if (valid == 5L) {
    qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
    start$mon <- qtr[start$mon + 1L]
    start$mday <- 1L
    start$isdst <- -1L
    maxx <- max(x, na.rm = TRUE)
    end <- as.POSIXlt(maxx)
    step <- if (length(by2) == 2L) 
      as.integer(by2[1L])
    else 1L
    end <- as.POSIXlt(end + (93 * step * 86400))
    end$mon <- qtr[end$mon + 1L]
    end$mday <- 1L
    end$isdst <- -1L
    breaks <- as.Date(seq(start, end, paste(step * 3L, 
                                            "months")))
    lb <- length(breaks)
    if (maxx < breaks[lb - 1]) 
      breaks <- breaks[-lb]
  }
  else {
    start <- as.Date(start)
    if (length(by2) == 2L) 
      incr <- incr * as.integer(by2[1L])
    maxx <- max(x, na.rm = TRUE)
    breaks <- seq(start, maxx + incr, breaks)
    breaks <- breaks[seq_len(1L + max(which(breaks <= 
                                              maxx)))]
  }
  if(offset == "end" || (is.logical(offset) && offset)) {
    breaks <- breaks + (x[length(x)] - breaks[length(breaks)])
  } else if (offset == "start") {
    breaks <- breaks + (x[1] - breaks[1])
  }
  breaks
}

bin_date <- function(time, breaks, offset){
  if(is.character(breaks) && length(breaks) == 1){
    breaks <- date_breaks(time, breaks, offset = offset)
  }
  bincode <- .bincode(unclass(as.Date(time)), unclass(breaks), right = FALSE)
  list(
    bin = bincode,
    breaks = breaks,
    complete_size = diff(breaks)
  )
}

#' Expand a dataset to include temporal aggregates
#' 
#' \lifecycle{experimental}
#' 
#' This feature is very experimental. It currently allows for temporal 
#' aggregation of daily data as a proof of concept.
#' 
#' @inheritParams aggregate_key
#' @param .window Temporal aggregations to include. The default (NULL) will
#' automatically identify appropriate temporal aggregations. This can be 
#' specified in several ways (see details).
#' @param .offset Offset the temporal aggregation windows to align with the start
#' or end of the data. If FALSE, no offset will be applied (giving common 
#' breakpoints for temporal bins.)
#' @param .bin_size Temporary. Define the number of observations in each temporal bucket
#' 
#' @details 
#' The aggregation `.window` can be specified in several ways:
#' * A character string, containing one of "day", "week", "month", "quarter" or 
#' "year". This can optionally be preceded by a (positive or negative) integer
#'  and a space, or followed by "s".
#' * A number, taken to be in days.
#' * A [`difftime`] object.
#' 
#' @examples
#' library(tsibble)
#' pedestrian %>%
#'   # Currently only supports daily data
#'   index_by(Date) %>% 
#'   dplyr::summarise(Count = sum(Count)) %>% 
#'   # Compute weekly aggregates
#'   fabletools:::aggregate_index("1 week", Count = sum(Count))
aggregate_index <- function(.data, .window, ..., .offset = "end", .bin_size = NULL){
  idx <- index_var(.data)
  # Compute temporal bins and bin sizes
  new_index <- bin_date(.data[[idx]], .window, .offset)
  if(!is.null(.bin_size)) new_index$complete_size <- vec_recycle(.bin_size, length(new_index$complete_size))
  as_tibble(.data) %>% 
    # Compute groups of temporal bins
    group_by(
      !!idx := !!{new_index$breaks[new_index$bin]},
      !!!key(.data)
    ) %>% 
    # Keep only complete windows, currently assumes daily base interval
    filter(dplyr::n() == (!!new_index$complete_size)[match((!!sym(idx))[1], !!new_index$breaks)]) %>%
    # Compute aggregates
    summarise(..., .groups = "drop") %>% 
    # Rebuild tsibble
    as_tsibble(key = key_vars(.data), index = !!index(.data))
}
