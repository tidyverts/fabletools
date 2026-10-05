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
