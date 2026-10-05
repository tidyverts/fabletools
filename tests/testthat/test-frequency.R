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

test_that("common_periods() for mixtime indices", {
  skip_if_no_granules()
  expect_equal(common_periods(freq_tsbl(mixtime::yearmonth(600:623))), c(year = 12))
  expect_equal(common_periods(freq_tsbl(mixtime::yearquarter(200:223))), c(year = 4))
  expect_equal(common_periods(freq_tsbl(mixtime::yearweek(2600:2623))), c(year = 52))
  expect_equal(
    common_periods(freq_tsbl(mixtime::date(d0 + 0:23))),
    c(year = 365.25, week = 7)
  )
  # Hourly mixtime intervals are measured in seconds
  expect_equal(
    common_periods(freq_tsbl(mixtime::datetime(t0 + 3600*(0:23)))),
    c(year = 8766, week = 168, day = 24)
  )
  expect_equal(
    common_periods(freq_tsbl(mixtime::datetime(t0 + 900*(0:23)))),
    c(year = 35064, week = 672, day = 96, hour = 4)
  )
  expect_equal(
    get_frequencies(c(mixtime::days(1L), mixtime::weeks(1L)),
                    freq_tsbl(mixtime::datetime(t0 + 3600*(0:23)))),
    c(24, 168)
  )
  expect_equal(
    get_frequencies("1 year", freq_tsbl(mixtime::datetime(t0 + 3600*(0:23)))),
    8766
  )
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
})
