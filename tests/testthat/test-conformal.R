context("test-conformal")

# conformal_scp() calibrates by repeatedly refit()ting an expanding window of
# history, so tests intentionally use short series/small horizons to keep
# runtime reasonable while still calibrating on a non-trivial number of
# out-of-sample errors.
us_deaths_small <- dplyr::filter(us_deaths_tr, index < tsibble::yearmonth("1976 Jan"))

test_that("conformal_scp() runs end-to-end and produces valid distributions", {
  skip_if_not_installed("fable")

  mbl_small <- us_deaths_small %>% model(ets = fable::ETS(value))

  set.seed(1)
  fc_cp <- mbl_small %>%
    mutate(ets = conformal_scp(ets, times = 200, min_calibration = 10)) %>%
    forecast(h = 3)

  expect_true(all(dist_types(fc_cp$value) == "dist_sample"))
  expect_equal(unique(lengths(distributional::parameters(fc_cp$value)$x)), 200)
  expect_equal(nrow(fc_cp), 3)
  expect_true(all(is.finite(fc_cp$.mean)))

  # Point forecasts (means) should be close to the analytical model's own
  # point forecasts - only the distribution's shape should differ.
  fbl_small <- mbl_small %>% forecast(h = 3)
  expect_equal(mean(fc_cp$.mean), mean(fbl_small$.mean), tolerance = 0.2)

  # Intervals should be extractable via hilo()/report().
  hl <- hilo(fc_cp, level = 80)
  expect_true(all(hl$"80%"$lower <= fc_cp$.mean))
  expect_true(all(hl$"80%"$upper >= fc_cp$.mean))
})

test_that("conformal_scp() calibration is genuinely out-of-sample (not just in-sample residuals)", {
  skip_if_not_installed("fable")

  mbl_small <- us_deaths_small %>% model(ets = fable::ETS(value))

  set.seed(1)
  fc_cp <- mbl_small %>%
    mutate(ets = conformal_scp(ets, times = 500, min_calibration = 10)) %>%
    forecast(h = 2)

  cp_width <- vapply(seq_len(2), function(i) {
    diff(range(distributional::parameters(fc_cp$value[[i]])$x[[1]]))
  }, double(1L))

  # The calibration pool for each horizon must come from genuinely
  # out-of-sample refits, not simply equal the model's in-sample training
  # residuals (which would be the anti-conservative shortcut this function
  # is explicitly required to avoid).
  naive_resid <- residuals(mbl_small$ets[[1]], type = "response")$.resid
  naive_resid <- naive_resid[!is.na(naive_resid)]

  pool_h1 <- fabletools:::conformal_cv_errors(mbl_small$ets[[1]], 1)[, 1]
  pool_h1 <- pool_h1[!is.na(pool_h1)]
  expect_false(isTRUE(all.equal(sort(pool_h1), sort(naive_resid))))

  # Sanity: calibrated intervals are finite and non-degenerate.
  expect_true(all(is.finite(cp_width)))
  expect_true(all(cp_width > 0))
})

test_that("conformal_scp() composes with mutate() across model columns/series and specification wrapping", {
  skip_if_not_installed("fable")

  two_id <- dplyr::bind_rows(
    dplyr::mutate(as_tibble(us_deaths_small), id = "A"),
    dplyr::mutate(as_tibble(us_deaths_small), id = "B")
  ) %>% as_tsibble(index = index, key = id)
  mbl_two_id <- two_id %>% model(ets = fable::ETS(value))

  fc_multi <- mbl_two_id %>%
    mutate(ets = conformal_scp(ets, times = 100, min_calibration = 10)) %>%
    forecast(h = 3)
  expect_true(all(dist_types(fc_multi$value) == "dist_sample"))
  expect_equal(n_keys(fc_multi), 2)

  # Applied to a model specification before fitting.
  fit_cp <- us_deaths_small %>%
    model(ets = conformal_scp(fable::ETS(value), times = 50, min_calibration = 10))
  expect_true(inherits(fit_cp$ets[[1]], "mdl_conformal_scp"))

  fc_cp <- fit_cp %>% forecast(h = 3)
  expect_true(all(dist_types(fc_cp$value) == "dist_sample"))
})

test_that("conformal_scp() errors sensibly when there isn't enough data to calibrate", {
  skip_if_not_installed("fable")

  short_series <- dplyr::filter(us_deaths_tr, index < tsibble::yearmonth("1974 Jun"))
  mbl_short <- short_series %>% model(ets = fable::ETS(value))

  expect_error(
    mbl_short %>%
      mutate(ets = conformal_scp(ets, min_calibration = 1000)) %>%
      forecast(h = 3),
    "Not enough held-out data"
  )
})
