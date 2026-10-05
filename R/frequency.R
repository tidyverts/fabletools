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
         with(list(secs = freq_sec/sum(as.numeric(unlist(x[nm]))*freq_sec[nm])), secs[secs>1])
  )
}

#' @rdname freq_tools
#' @exportS3Method common_periods "mixtime::mt_unit"
`common_periods.mixtime::mt_unit` <- function(x){
  common_periods_granules(list(x))
}

#' @rdname freq_tools
#' @export
common_periods.list <- function(x){
  granules <- interval_granules(x)
  if(is.null(granules)) {
    abort("A list of time units (a composite interval) is required to compute common periods.")
  }
  common_periods_granules(granules)
}

# Common periods of an interval made from mixtime granules (time units).
# Fixed-length relationships come from mixtime, while variable-length ones
# (days in a year, weeks in a year) use the usual approximations.
common_periods_granules <- function(x){
  if(is_empty(x) || any(map_lgl(x, function(g) is.na(g@n)))) return(NULL)
  cal <- mixtime::cal_gregorian
  
  per_day <- granules_per_unit(x, cal$day(1L))
  if(!is.null(per_day)) {
    if(all(map_lgl(x, inherits, "mixtime::tu_week"))) {
      return(c("year" = 52 * 7 * per_day))
    }
    if(per_day <= 1) {
      return(c("year" = 365.25, "week" = 7) * per_day)
    }
    freq <- c(year = 365.25, week = 7, day = 1, hour = 1/24, minute = 1/1440,
              second = 1/86400, millisecond = 1/864e5, microsecond = 1/864e8,
              nanosecond = 1/864e11) * per_day
    return(freq[freq > 1])
  }
  
  per_year <- granules_per_unit(x, cal$year(1L))
  if(!is.null(per_year)) {
    return(c("year" = per_year))
  }
  
  # Time units without a calendar (e.g. integer indices)
  c("none" = 1)
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
  frequencies <- Filter(function(x) x >= 1, common_periods(data))
  if(is_empty(frequencies)) frequencies <- 1
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
  check_installed("lubridate")
  m <- lubridate::as.period(period)
  if(is.na(m)) abort(paste("Unknown period:", period))
  get_frequencies(m, data, ...)
}

#' @rdname freq_tools
#' @export
get_frequencies.Period <- function(period, data, ...){
  check_installed("lubridate")
  
  interval <- tsibble::interval(data)
  granules <- interval_granules(interval)
  
  interval <- if(is.null(granules)) {
    with(interval, lubridate::years(year) + 
      lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
      lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) + 
      lubridate::seconds(second) + lubridate::milliseconds(millisecond) + 
      lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
  } else {
    reduce(map(granules, granule_as_period), `+`)
  }
  
  suppressMessages(period / interval)
}

#' @rdname freq_tools
#' @exportS3Method get_frequencies "mixtime::mixtime"
`get_frequencies.mixtime::mixtime` <- function(period, data, ...){
  if(!all(mixtime::time_is_duration(period))) {
    abort("A mixtime `period` must be a duration, such as `mixtime::years(1L)`.")
  }
  granules <- interval_granules(tsibble::interval(data))
  map_dbl(seq_along(period), function(i) {
    p <- period[i]
    unit <- mixtime::chronon_glb(p)
    n <- if(!is.null(granules)) granules_per_unit(granules, unit)
    if(!is.null(n)) return(as.double(p) * n)
    # Variable-length relationships (e.g. days in a year) are approximated
    check_installed("lubridate")
    get_frequencies(granule_as_period(unit) * as.double(p), data, ...)
  })
}

# Mixtime time units (granules) of a tsibble interval as a list, or NULL for a
# legacy tsibble interval object.
interval_granules <- function(x){
  if(inherits(x, "mixtime::mt_unit")) return(list(x))
  if(is.list(x) && !is.object(x) && all(map_lgl(x, inherits, "mixtime::mt_unit"))) {
    return(x)
  }
  NULL
}

# The number of observations spaced by the granules that fit in `unit`, or
# NULL if this isn't fixed (e.g. days in a month).
granules_per_unit <- function(granules, unit){
  n <- tryCatch(
    1/sum(1/map_dbl(granules, mixtime::chronon_cardinality, unit)),
    error = function(e) NULL
  )
  if(length(n) == 1 && is.finite(n)) n else NULL
}

granule_as_period <- function(x){
  n <- x@n
  unit <- sub("^mixtime::tu_", "", class(x)[1])
  switch(unit,
    year = lubridate::years(n),
    quarter = lubridate::period(3*n, units = "month"),
    month = lubridate::period(n, units = "month"),
    week = lubridate::weeks(n),
    day = lubridate::days(n),
    hour = lubridate::hours(n),
    minute = lubridate::minutes(n),
    second = lubridate::seconds(n),
    millisecond = lubridate::milliseconds(n),
    microsecond = lubridate::microseconds(n),
    nanosecond = lubridate::nanoseconds(n),
    abort(sprintf("Cannot convert a time unit of class <%s> into a period.", class(x)[1]))
  )
}
