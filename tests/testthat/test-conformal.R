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
    mutate(ets = conformal_scp(ets, n_quantiles = 200, min_calibration = 10)) %>%
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
    mutate(ets = conformal_scp(ets, n_quantiles = 500, min_calibration = 10)) %>%
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

  pool_h1 <- fabletools:::conformal_cv_errors(mbl_small$ets[[1]], 1)$errors[, 1]
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
    mutate(ets = conformal_scp(ets, n_quantiles = 100, min_calibration = 10)) %>%
    forecast(h = 3)
  expect_true(all(dist_types(fc_multi$value) == "dist_sample"))
  expect_equal(n_keys(fc_multi), 2)

  # Applied to a model specification before fitting.
  fit_cp <- us_deaths_small %>%
    model(ets = conformal_scp(fable::ETS(value), n_quantiles = 50, min_calibration = 10))
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

test_that("conformal_scp() refits models with regressors from their specification", {
  skip_if_not_installed("fable")

  set.seed(1)
  dt <- tsibble::tsibble(t = 1:40, x = rnorm(40), index = t)
  dt$y <- 2 * dt$x + cumsum(rnorm(40))
  future <- dplyr::mutate(tsibble::new_data(dt, 3), x = 0)

  fit <- model(dt, lm = conformal_scp(fable::TSLM(y ~ x), n_quantiles = 50, min_calibration = 10))
  expect_named(attr(fit$lm[[1]], "conformal_regressors"), "x")
  fc <- forecast(fit, new_data = future)
  expect_true(all(dist_types(fc$y) == "dist_sample"))
  expect_true(all(is.finite(fc$.mean)))

  # The regressors are updated by refit() and stream()
  expect_length(attr(refit(fit, dt[1:30, ])$lm[[1]], "conformal_regressors")$x, 30)
  streamed <- stream(refit(fit, dt[1:30, ]), dt[31:40, ])
  expect_equal(attr(streamed$lm[[1]], "conformal_regressors")$x, dt$x)

  # An estimated model doesn't keep its regressors
  fit <- model(dt, lm = fable::TSLM(y ~ x))
  expect_error(
    forecast(mutate(fit, lm = conformal_scp(lm, min_calibration = 10)), new_data = future),
    "apply `conformal_scp\\(\\)` to the model specification"
  )
})

test_that("conformal_scp() calibrates each horizon with enough errors", {
  skip_if_not_installed("fable")

  set.seed(1)
  rw <- model(tsibble::tsibble(t = 1:40, y = cumsum(rnorm(40)), index = t), rw = fable::RW(y))

  # Horizons with fewer than 20 errors can't be calibrated
  expect_warning(
    fc <- forecast(mutate(rw, rw = conformal_scp(rw, n_quantiles = 50)), h = 25),
    "horizons 21 to 25 are `NA`"
  )
  expect_true(all(is.finite(fc$.mean[1:20])))
  expect_true(all(is.na(fc$.mean[21:25])))

  # Calibration needs enough errors in total
  short <- model(tsibble::tsibble(t = 1:15, y = cumsum(rnorm(15)), index = t), rw = fable::RW(y))
  expect_warning(
    fc <- forecast(mutate(short, rw = conformal_scp(rw, n_quantiles = 50)), h = 5),
    "horizons 1 to 5 are `NA`"
  )
  expect_true(all(is.na(fc$.mean)))
  expect_error(
    forecast(mutate(short, rw = conformal_scp(rw, min_calibration = 200)), h = 5),
    "only 60 were available"
  )
})

test_that("conformal_scp() calibrates transformed responses on their original scale", {
  skip_if_not_installed("fable")

  set.seed(1)
  dt <- tsibble::tsibble(t = 1:40, y = exp(cumsum(rnorm(40, 0, 0.1))), index = t)
  fit <- model(dt, rw = fable::RW(log(y)))
  fc <- forecast(mutate(fit, rw = conformal_scp(rw, n_quantiles = 50, min_calibration = 10)), h = 3)
  expect_true(all(dist_types(fc$y) == "dist_sample"))

  # The calibration errors are those of the back-transformed refits (with
  # bias adjusted point forecasts)
  errors <- fabletools:::conformal_cv_errors(fit$rw[[1]], 1)$errors[, 1]
  expect_equal(errors[-1], diff(dt$y), tolerance = 0.1)
})
