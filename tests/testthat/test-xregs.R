context("test-xregs.R")

xreg_tsbl <- function(idx, ...) {
  tsibble::tsibble(t = idx, y = seq_along(idx), index = t, ...)
}

test_that("trend() knots and origin must match the index", {
  x <- xreg_tsbl(as.Date("2020-01-01") + 0:9)
  expect_equal(fbl_trend(x, origin = as.Date("2020-01-01"))$trend, 1:10)
  expect_equal(
    fbl_trend(x, knots = as.Date("2020-01-05"), origin = x$t[1])[[2]],
    c(rep(0, 5), 1:5)
  )
  expect_error(fbl_trend(x, knots = "2020-01-05"), "knots")
  expect_error(fbl_trend(x, origin = 18262), "origin")

  # Compatible classes are converted
  x <- xreg_tsbl(as.POSIXct("2020-01-01", tz = "UTC") + 3600*(0:9))
  expect_equal(fbl_trend(x, origin = as.Date("2020-01-01"))$trend, 1:10)
})

test_that("trend() accepts fractional knots for integer indices", {
  x <- xreg_tsbl(2000:2005)
  expect_equal(
    fbl_trend(x, knots = 2002.5, origin = x$t[1])[[2]],
    c(0, 0, 0, 0.5, 1.5, 2.5)
  )
  expect_equal(fbl_trend(x, origin = 1999.5)$trend, 1.5:6.5)
})

test_that("trend() knots and origin must match a mixtime index's resolution", {
  skip_if_not_installed("mixtime")
  x <- xreg_tsbl(mixtime::yearmonth(600:611))
  skip_if_not(
    inherits(tsibble::interval(x), "mixtime::mt_unit"),
    "tsibble intervals are not mixtime time units"
  )
  expect_equal(fbl_trend(x, origin = mixtime::yearmonth(600L))$trend, 1:12)
  expect_equal(
    fbl_trend(x, knots = mixtime::yearmonth(605L), origin = x$t[1])[[2]],
    c(rep(0, 6), 1:6)
  )
  expect_error(
    fbl_trend(x, origin = mixtime::date(as.Date("2020-01-01"))),
    "same time resolution"
  )
})

test_that("trend() is proportional to elapsed time for irregular data", {
  x <- xreg_tsbl(c(1, 3, 4, 9), regular = FALSE)
  expect_equal(fbl_trend(x, origin = 1)$trend, c(1, 3, 4, 9))
  expect_equal(fbl_trend(x, knots = 4, origin = 1)[[2]], c(0, 0, 0, 5))
  
  x <- xreg_tsbl(as.Date("2020-01-01") + c(0, 2, 3, 8), regular = FALSE)
  expect_equal(fbl_trend(x, origin = x$t[1])$trend, c(1, 3, 4, 9))
  
  # A single observation has no known interval
  x <- xreg_tsbl(as.Date("2020-01-01"))
  expect_equal(fbl_trend(x, origin = x$t[1])$trend, 1)
})

test_that("trend() origin is set for each series and dataset", {
  skip_if_not_installed("fable")

  spec <- fable::TSLM(value ~ trend())
  annual <- tsibble::tsibble(year = 2001:2020, value = 2 * (1:20), index = year)
  fit_month <- model(us_deaths, spec)
  # A later dataset with a different index class uses its own origin
  expect_no_error(fit_year <- model(annual, spec))
  expect_identical(attr(fit_year[[1]][[1]], "origin"), 2001L)
  expect_identical(attr(fit_month[[1]][[1]], "origin"), us_deaths$index[1])
  expect_equal(tidy(fit_year)$estimate, c(0, 2))

  # Series starting at different times are each forecast with their own origin
  lung_deaths_ragged <- lung_deaths_long_tr %>%
    dplyr::filter(key == "mdeaths" | index >= tsibble::yearmonth("1975 Jan"))
  fit <- model(lung_deaths_ragged, lm = spec)
  fc <- forecast(fit, h = 6)
  for (k in c("mdeaths", "fdeaths")) {
    series <- dplyr::filter(lung_deaths_ragged, key == k)
    fc_k <- forecast(model(series, lm = fable::TSLM(value ~ trend())), h = 6)
    expect_equal(dplyr::filter(fc, key == k)$.mean, fc_k$.mean)
    expect_equal(
      attr(dplyr::filter(fit, key == k)$lm[[1]], "origin"),
      series$index[1]
    )
  }
})

test_that("fourier(origin) is deprecated and ignored", {
  x <- xreg_tsbl(as.Date("2020-01-01") + 0:13)
  fourier <- common_xregs$fourier
  environment(fourier) <- list2env(list(self = list(data = x)), parent = environment(fourier))
  expected <- fourier(period = 7, K = 2)
  lifecycle::expect_deprecated(
    res <- fourier(period = 7, K = 2, origin = x$t[3])
  )
  expect_equal(res, expected)
})
