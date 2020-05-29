#' Extract frequencies for common seasonal periods
#'
#' @param x An object containing temporal data (such as a `tsibble`, `interval`, `datetime` and others.)
#'
#' @return A named vector of frequencies appropriate for the provided data.
#'
#' @references <https://robjhyndman.com/hyndsight/seasonal-periods/>
#'
#' @rdname freq_tools
#'
#' @examples
#' common_periods(tsibble::pedestrian)
#'
#' @export
common_periods <- function(x){
  UseMethod("common_periods")
}

#' @rdname freq_tools
#' @export
common_periods.default <- function(x){
  common_periods(interval_pull(x))
}

#' @rdname freq_tools
#' @export
common_periods.tbl_ts <- function(x){
  common_periods(tsibble::interval(x))
}

#' @rdname freq_tools
#' @export
common_periods.interval <- function(x){
  if(inherits(x, "vctrs_vctr")){
    x <- vctrs::vec_data(x)
  }
  freq_sec <- c(year = 31557600, week = 604800, day = 86400, hour = 3600, minute = 60, second = 1,
                millisecond = 1e-3, microsecond = 1e-6, nanosecond = 1e-9)
  nm <- names(x)[x!=0]
  if(is_empty(x)) return(NULL)
  switch(paste(nm, collapse = ""),
         "unit" = c("none" = 1),
         "year" = c("year" = 1),
         "quarter" = c("year" = 4/x[["quarter"]]),
         "month" = c("year" = 12/x[["month"]]),
         "week" = c("year" = 52/x[["week"]]),
         "day" = c("year" = 365.25, "week" = 7)/x[["day"]],
         with(list(secs = freq_sec/sum(as.numeric(x)*freq_sec[nm])), secs[secs>1])
  )
}

#' @rdname freq_tools
#' @param period Specification of the time-series period
#' @param ... Other arguments to be passed on to methods
#' @export
get_frequencies <- function(period, ...){
  UseMethod("get_frequencies")
}

#' @rdname freq_tools
#' @export
get_frequencies.numeric <- function(period, ...){
  period
}

#' @rdname freq_tools
#' @param data A tsibble
#' @param .auto The method used to automatically select the appropriate seasonal
#' periods
#' @export
get_frequencies.NULL <- function(period, data, ...,
                                 .auto = c("smallest", "largest", "all")){
  .auto <- match.arg(.auto)
  frequencies <- common_periods(data) %||% 1
  if(.auto == "smallest") {
    return(frequencies[which.min(frequencies)])
  }
  else if(.auto == "largest"){
    return(frequencies[which.max(frequencies)])
  }
  else {
    return(frequencies)
  }
}

#' @rdname freq_tools
#' @export
get_frequencies.character <- function(period, data, ...){
  require_package("lubridate")
  get_frequencies(lubridate::as.period(period), data, ...)
}

#' @rdname freq_tools
#' @export
get_frequencies.Period <- function(period, data, ...){
  require_package("lubridate")
  
  interval <- tsibble::interval(data)
  
  interval <- with(interval, lubridate::years(year) + 
    lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
    lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) + 
    lubridate::seconds(second) + lubridate::milliseconds(millisecond) + 
    lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
  
  suppressMessages(period / interval)
}