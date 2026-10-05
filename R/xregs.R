# The size of one interval step, measured in units of `as.double(index)`.
index_time_units <- function(x) {
  idx <- x[[index_var(x)]]
  itvl <- interval(x)
  granules <- interval_granules(itvl)
  if (inherits(idx, "mixtime::mixtime") && !is.null(granules)) {
    # Count chronons (the units of `as.double()`) per interval step, since the
    # interval's time unit can differ from the index's chronon.
    chronon <- mixtime::chronon_glb(idx)
    n <- tryCatch(
      sum(map_dbl(granules, function(g) mixtime::chronon_cardinality(chronon, g))),
      error = function(e) NULL
    )
    if (length(n) == 1 && is.finite(n)) return(n)
  }
  default_time_units(itvl)
}

# Times (such as trend() knots and origin) as numbers on the same scale as
# `as.double()` of the index, after checking they are compatible with it.
index_time_as_double <- function(x, time, arg) {
  idx <- x[[index_var(x)]]
  time <- vctrs::vec_cast(time, vctrs::vec_ptype(idx), x_arg = arg)
  if (inherits(idx, "mixtime::mixtime") && length(time) > 0) {
    # mixtime values of the same class can have a different chronon (e.g. a
    # date cast to a year-month), which changes the scale of `as.double()`
    idx_chronon <- mixtime::chronon_glb(idx)
    time_chronon <- mixtime::chronon_glb(time)
    if (!identical(class(time_chronon), class(idx_chronon)) ||
        time_chronon@n != idx_chronon@n) {
      abort(sprintf(
        "`%s` must have the same time resolution as the index (%s), not %s.",
        arg, format_granule(idx_chronon), format_granule(time_chronon)
      ))
    }
  }
  as.double(time)
}

format_granule <- function(x) {
  paste(x@n, sub("^mixtime::(tu_)?", "", class(x)[1]))
}

fbl_trend <- function(x, knots = NULL, origin = NULL) {
  idx_num <- as.double(x[[index_var(x)]])
  knots_num <- if (is.null(knots)) {
    NULL
  } else {
    index_time_as_double(x, knots, "knots")
  }
  index_interval <- index_time_units(x)
  if (!isTRUE(index_interval > 0)) {
    # Without a known interval (e.g. irregular data), the trend is
    # proportional to elapsed time in the units of the index
    index_interval <- 1
  }
  idx_num <- idx_num / index_interval
  knots_num <- knots_num / index_interval
  if (!is.null(origin)) {
    # trend should count from 1
    origin <- index_time_as_double(x, origin, "origin") / index_interval - 1
    idx_num <- idx_num - origin
    knots_num <- knots_num - origin
  }
  
  knots_exprs <- map(knots_num, function(.x) pmax(0, idx_num - .x))
  knots_exprs <- set_names(
    knots_exprs, 
    map_chr(knots_num, function(.x) paste0("trend_", format(.x)))
  )
  tibble(
    trend = idx_num,
    !!!knots_exprs
  )
}

fbl_season <- function(x, period) {
  idx_num <- as.double(x[[index_var(x)]])
  index_interval <- index_time_units(x)
  idx_num <- idx_num / index_interval
  period <- get_frequencies(period, x, .auto = "smallest")
  season_exprs <- map(period, function(.x) expr(factor(floor((idx_num %% (!!.x)) + 1), levels = seq_len(!!.x))))
  season_exprs <- set_names(season_exprs, names(period) %||% paste0("season_", period))
  tibble(!!!season_exprs)
}

fbl_fourier <- function(x, period, K) {
  idx_num <- as.double(x[[index_var(x)]])
  index_interval <- index_time_units(x)
  idx_num <- idx_num / index_interval
  period <- get_frequencies(period, x, .auto = "smallest")
  
  if (length(period) != length(K)) {
    abort("Number of periods does not match number of orders")
  }
  if (any(2 * K > period)) {
    abort("K must be not be greater than period/2")
  }
  
  fourier_exprs <- map2(
    as.numeric(period), K,
    function(period, K) {
      set_names(seq_len(K) / period, paste0(seq_len(K), "_", round(period)))
    }
  ) %>%
    invoke(c, .) %>%
    .[!duplicated(.)] %>%
    map2(., names(.), function(p, name) {
      out <- exprs(C = cospi(2 * !!p * idx_num))
      if (abs(2 * p - round(2 * p)) > .Machine$double.eps) {
        out <- c(out, exprs(S = sinpi(2 * !!p * idx_num)))
      }
      names(out) <- paste0(names(out), name)
      out
    }) %>%
    set_names(NULL) %>%
    unlist(recursive = FALSE)
  
  tibble(!!!fourier_exprs)
}

#' Common exogenous regressors
#'
#' These special functions provide interfaces to more complicated functions within
#' the model formulae interface.
#'
#' @aliases trend season fourier
#' 
#' @section Specials:
#'
#' \subsection{trend}{
#' The `trend` special includes common linear trend regressors in the model. It also supports piecewise linear trend via the `knots` argument. For irregular time series, the trend is proportional to elapsed time, measured in the units of the time index (e.g. days for `Date`, seconds for `POSIXct`).
#' \preformatted{
#' trend(knots = NULL, origin = NULL)
#' }
#'
#' \tabular{ll}{
#'   `knots`    \tab A vector of times (same class as the data's time index) identifying the position of knots for a piecewise linear trend.\cr
#'   `origin`   \tab An optional time value (same class as the data's time index) to act as the starting time for the trend.
#' }
#' }
#'
#' \subsection{season}{
#' The `season` special includes seasonal dummy variables in the model.
#' \preformatted{
#' season(period = NULL)
#' }
#'
#' \tabular{ll}{
#'   `period`   \tab The periodic nature of the seasonality. This can be either a number indicating the number of observations in each seasonal period, or text to indicate the duration of the seasonal window (for example, annual seasonality would be "1 year").
#' }
#' }
#'
#' \subsection{fourier}{
#' The `fourier` special includes seasonal fourier terms in the model. The maximum order of the fourier terms must be specified using `K`.
#' \preformatted{
#' fourier(period = NULL, K)
#' }
#'
#' \tabular{ll}{
#'   `period`   \tab The periodic nature of the seasonality. This can be either a number indicating the number of observations in each seasonal period, or text to indicate the duration of the seasonal window (for example, annual seasonality would be "1 year"). \cr
#'   `K`        \tab The maximum order of the fourier terms.
#' }
#' }
#'
#' @format NULL
#' 
#' @export
common_xregs <- list(
  trend = function(knots = NULL, origin = NULL) {
    if (is.null(origin)) {
      if (is.null(self$origin)) {
        self$origin <- self$data[[index_var(self$data)]][1]
      }
      origin <- self$origin
    }
    as.matrix(fabletools:::fbl_trend(self$data, knots, origin))
  },
  season = function(period = NULL) {
    out <- as_model_matrix(fabletools:::fbl_season(self$data, period))
    stats::model.matrix(~., data = out)[, -1, drop = FALSE]
  },
  fourier = function(period = NULL, K) {
    as.matrix(fabletools:::fbl_fourier(self$data, period, K))
  }
)
