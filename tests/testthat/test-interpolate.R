context("test-interpolate.R")

test_that("Test interpolation", {
  skip_if_not_installed("fable")
  NA_pos <- c(0, sample(seq_len(NROW(us_deaths)), 10), NROW(us_deaths))
  us_deaths$value[NA_pos] <- NA
  mbl_miss <- us_deaths %>%
    model(fable::TSLM(value ~ trend() + season()))
  interpolated <- interpolate(mbl_miss, us_deaths)
  expect_true(all(!is.na(interpolated$value)))
})

test_that("interpolate() requires a single model", {
  skip_if_not_installed("fable")

  expect_error(interpolate(mbl_complex, lung_deaths_long_tr), "only be done using one model")
  mbl_grp <- dplyr::transmute(mbl_complex, grp = cbind(ets, lm))
  expect_error(interpolate(mbl_grp, lung_deaths_long_tr), "only be done using one model")
})
