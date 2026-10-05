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
