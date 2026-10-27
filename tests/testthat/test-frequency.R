context("test-frequency.R")

freq_tsbl <- function(idx) {
  tsibble::tsibble(t = idx, y = seq_along(idx), index = t)
}
d0 <- as.Date("2020-01-01")
t0 <- as.POSIXct("2020-01-01", tz = "UTC")
# Legacy tsibble index classes (soft-deprecated with mixtime-based tsibble)
ym0 <- suppressWarnings(tsibble::yearmonth(d0))
yq0 <- suppressWarnings(tsibble::yearquarter(d0))
yw0 <- suppressWarnings(tsibble::yearweek(d0))
# Sub-daily mixtime indices (mixtime::datetime() has a 1 second chronon)
mt_hourly <- function(x) mixtime::linear_time(x, chronon = hour(1L))
mt_15min <- function(x) mixtime::linear_time(x, chronon = minute(15L))

# Skip unless tsibble intervals are mixtime time units (granules)
skip_if_no_granules <- function() {
  skip_if_not_installed("mixtime")
  skip_if_not(
    inherits(tsibble::interval(freq_tsbl(1:2)), "mixtime::mt_unit"),
    "tsibble intervals are not mixtime time units"
  )
}

test_that("common_periods() for legacy index classes", {
  expect_equal(common_periods(freq_tsbl(ym0 + 0:23)), c(year = 12))
  expect_equal(common_periods(freq_tsbl(yq0 + 0:23)), c(year = 4))
  expect_equal(common_periods(freq_tsbl(yw0 + 0:23)), c(year = 52))
  expect_equal(common_periods(freq_tsbl(yw0 + 2*(0:23))), c(year = 26))
  expect_equal(common_periods(freq_tsbl(d0 + 0:23)), c(year = 365.25, week = 7))
  expect_equal(common_periods(freq_tsbl(d0 + 2*(0:23))), c(year = 365.25, week = 7)/2)
  expect_equal(
    common_periods(freq_tsbl(t0 + 3600*(0:23))),
    c(year = 8766, week = 168, day = 24)
  )
  expect_equal(
    common_periods(freq_tsbl(t0 + 5400*(0:23))),
    c(year = 5844, week = 112, day = 16)
  )
  expect_equal(common_periods(freq_tsbl(2000:2020)), c(year = 1))
  expect_equal(common_periods(freq_tsbl(1:20)), c(none = 1))
  expect_equal(common_periods(d0 + 0:23), c(year = 365.25, week = 7))
})

test_that("get_frequencies() with text periods", {
  expect_equal(get_frequencies("1 year", freq_tsbl(ym0 + 0:23)), 12)
  expect_equal(get_frequencies("2 years", freq_tsbl(yq0 + 0:23)), 8)
  expect_equal(get_frequencies("1 week", freq_tsbl(t0 + 3600*(0:23))), 168)
  expect_equal(get_frequencies("1 year", freq_tsbl(d0 + 0:23)), 365.25)
  expect_equal(
    get_frequencies(NULL, freq_tsbl(t0 + 3600*(0:23)), .auto = "smallest"),
    c(day = 24)
  )
})

test_that("get_frequencies() with mixtime durations", {
  skip_if_not_installed("mixtime")
  ym <- freq_tsbl(ym0 + 0:23)
  expect_equal(get_frequencies(mixtime::years(2L), ym), 24)
  expect_equal(get_frequencies(mixtime::weeks(1L), freq_tsbl(t0 + 3600*(0:23))), 168)
  # Variable length relationships use the usual approximations
  expect_equal(get_frequencies(mixtime::years(1L), freq_tsbl(d0 + 0:23)), 365.25)
  expect_error(get_frequencies(mixtime::yearmonth(600L), ym), "duration")
})

test_that("multi-year, numeric and irregular intervals", {
  # These results are the same for legacy and mixtime-based tsibbles
  yr2 <- freq_tsbl(seq(2000, 2020, by = 2))
  expect_equal(unname(get_frequencies(NULL, yr2)), 1)
  expect_equal(get_frequencies("4 years", yr2), 2)
  expect_equal(NROW(make_future_data(yr2, "4 years")), 2)
  expect_equal(tsibble::new_data(yr2, 3)$t, c(2022, 2024, 2026))
  expect_equal(fbl_trend(yr2, origin = 2000)$trend, 1:11)
  expect_equal(as.integer(fbl_season(yr2, 2)[[1]]), rep_len(1:2, 11))
  # Two-yearly data has half an observation per year
  expect_equal(
    common_periods(yr2),
    if (inherits(tsibble::interval(yr2), "mixtime::mt_unit")) c(year = 0.5) else c(year = 1)
  )

  for (half in list(freq_tsbl(seq(0, 5, by = 0.5)), freq_tsbl(seq(0, 30, by = 3)))) {
    expect_equal(common_periods(half), c(none = 1))
    expect_equal(get_frequencies(NULL, half), c(none = 1))
    # Numeric steps have no length in time
    expect_equal(get_frequencies("1 year", half), Inf)
    expect_equal(fbl_trend(half, origin = half$t[1])$trend, 1:11)
  }
  expect_equal(tsibble::new_data(freq_tsbl(seq(0, 5, by = 0.5)), 2)$t, c(5.5, 6))

  irr <- tsibble::tsibble(
    t = d0 + c(0, 1, 3, 6, 10), y = 1:5, index = t, regular = FALSE
  )
  expect_equal(get_frequencies("1 year", irr), Inf)
  expect_null(calendar_horizon(irr, "1 year"))
  expect_error(make_future_data(irr, "1 year"), "irregular")
  expect_equal(fbl_trend(irr)$trend, as.double(irr$t))
})

test_that("common_periods() for mixtime indices", {
  skip_if_no_granules()
  expect_equal(common_periods(freq_tsbl(mixtime::yearmonth(600:623))), c(year = 12))
  expect_equal(common_periods(freq_tsbl(mixtime::yearquarter(200:223))), c(year = 4))
  expect_equal(common_periods(freq_tsbl(mixtime::yearweek(2600:2623))), c(year = 52))
  expect_equal(
    common_periods(freq_tsbl(mixtime::date(d0 + 0:23))),
    c(year = 365.25, week = 7)
  )
  # Sub-daily mixtime indices take their interval from the chronon
  expect_equal(
    common_periods(freq_tsbl(mt_hourly(t0 + 3600*(0:23)))),
    c(year = 8766, week = 168, day = 24)
  )
  expect_equal(
    common_periods(freq_tsbl(mt_15min(t0 + 900*(0:23)))),
    c(year = 35064, week = 672, day = 96, hour = 4)
  )
  expect_equal(
    get_frequencies(c(mixtime::days(1L), mixtime::weeks(1L)),
                    freq_tsbl(mt_hourly(t0 + 3600*(0:23)))),
    c(24, 168)
  )
  expect_equal(
    get_frequencies("1 year", freq_tsbl(mt_hourly(t0 + 3600*(0:23)))),
    8766
  )
})

test_that("trend(), season() and fourier() match for mixtime and base indices", {
  skip_if_no_granules()
  compare_xregs <- function(base, mt, period) {
    x <- freq_tsbl(base)
    y <- freq_tsbl(mt)
    expect_equal(fbl_trend(y, origin = mt[1]), fbl_trend(x, origin = base[1]))
    expect_equal(fbl_season(y, period), fbl_season(x, period))
    expect_equal(
      fbl_fourier(y, period, K = 2),
      fbl_fourier(x, period, K = 2)
    )
  }
  compare_xregs(d0 + 0:30, mixtime::date(d0 + 0:30), period = 7)
  compare_xregs(t0 + 3600*(0:60), mt_hourly(t0 + 3600*(0:60)), period = 24)
  compare_xregs(t0 + 900*(0:200), mt_15min(t0 + 900*(0:200)), period = 96)

  # Trend counts interval steps from the origin
  x <- freq_tsbl(mt_15min(t0 + 900*(0:10)))
  expect_equal(fbl_trend(x, origin = x$t[1])$trend, 1:11)
})

test_that("models and forecasts with a mixtime index", {
  skip_if_no_granules()
  skip_if_not_installed("fable")
  y <- tsibble::tsibble(
    t = mixtime::yearmonth(600:647), y = as.numeric(USAccDeaths[1:48]),
    index = t
  )
  fit <- model(y,
    snaive = fable::SNAIVE(y ~ lag("year")),
    lm = fable::TSLM(y ~ trend() + season())
  )
  expect_false(any(vapply(fit$snaive, is_null_model, logical(1))))
  expect_false(any(vapply(fit$lm, is_null_model, logical(1))))
  expect_equal(nrow(forecast(fit, h = "2 years")), 48)
  expect_equal(nrow(forecast(fit, h = mixtime::years(1L))), 24)
  # The trend origin keeps the index class
  expect_identical(fit$lm[[1]]$model$origin, y$t[1])
})

test_that("text horizons cover the calendar period after the data", {
  daily <- function(start) {
    tsibble::tsibble(t = as.Date(start) + 0:99, y = 1, index = t)
  }
  # The year after 2024-02-03 contains 29 Feb 2024
  expect_equal(NROW(make_future_data(daily("2023-10-28"), "1 year")), 366)
  expect_equal(NROW(make_future_data(daily("2024-10-28"), "1 year")), 365)
  expect_equal(NROW(make_future_data(daily("2023-10-28"), "1 month")), 29)
  expect_equal(NROW(make_future_data(daily("2023-10-28"), "2 weeks")), 14)
  expect_equal(NROW(make_future_data(daily("2023-10-28"), 10)), 10)

  hourly <- tsibble::tsibble(
    t = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * (0:99), y = 1, index = t
  )
  expect_equal(NROW(make_future_data(hourly, "1 month")), 31 * 24)

  # Other intervals are unchanged
  expect_equal(NROW(make_future_data(as_tsibble(USAccDeaths), "1 year")), 12)
})
