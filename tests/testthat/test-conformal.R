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

test_that("conformal_scp() errors for models with regressors", {
  skip_if_not_installed("fable")

  set.seed(1)
  dt <- tsibble::tsibble(t = 1:40, x = rnorm(40), index = t)
  dt$y <- 2 * dt$x + cumsum(rnorm(40))

  fit <- model(dt, lm = fable::TSLM(y ~ x), trend = fable::TSLM(y ~ trend()))
  expect_error(
    mutate(fit, lm = conformal_scp(lm)),
    "exogenous regressors\\) aren't supported yet"
  )
  # Specials computed from the response's history are fine
  expect_s3_class(mutate(fit, trend = conformal_scp(trend))$trend[[1]], "mdl_conformal_scp")

  # Applied to the specification, estimation fails with the same error
  expect_error(
    model(dt, lm = conformal_scp(fable::TSLM(y ~ x)), .safely = FALSE),
    "exogenous regressors\\) aren't supported yet"
  )
  expect_warning(
    fit <- model(dt, lm = conformal_scp(fable::TSLM(y ~ x))),
    "aren't supported yet"
  )
  expect_true(is_null_model(fit$lm[[1]]))
})

test_that("conformal_scp() calibrates each horizon with enough errors", {
  skip_if_not_installed("fable")

  set.seed(1)
  rw <- model(tsibble::tsibble(t = 1:40, y = cumsum(rnorm(40)), index = t), rw = fable::RW(y))

  # Horizons with fewer than 20 errors can't be calibrated
  expect_warning(
    fc <- forecast(mutate(rw, rw = conformal_scp(rw, n_quantiles = 50, initial = 1)), h = 25),
    "horizons 21 to 25 are `NA`"
  )
  expect_true(all(is.finite(fc$.mean[1:20])))
  expect_true(all(is.na(fc$.mean[21:25])))
  # Refits starting from 10 observations leave fewer errors
  expect_warning(
    forecast(mutate(rw, rw = conformal_scp(rw, n_quantiles = 50)), h = 25),
    "horizons 12 to 25 are `NA`"
  )

  # Calibration needs enough errors in total
  short <- model(tsibble::tsibble(t = 1:15, y = cumsum(rnorm(15)), index = t), rw = fable::RW(y))
  expect_warning(
    fc <- forecast(mutate(short, rw = conformal_scp(rw, n_quantiles = 50, initial = 1)), h = 5),
    "horizons 1 to 5 are `NA`"
  )
  expect_true(all(is.na(fc$.mean)))
  expect_error(
    forecast(mutate(short, rw = conformal_scp(rw, min_calibration = 200, initial = 1)), h = 5),
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

test_that("conformal_scp() refits from an initial window", {
  skip_if_not_installed("fable")

  set.seed(1)
  dt <- tsibble::tsibble(t = 1:40, y = cumsum(rnorm(40)), index = t)
  fit <- model(dt, rw = fable::RW(y ~ drift()))

  # The first refit uses the first `initial` observations
  errors <- fabletools:::conformal_cv_errors(fit$rw[[1]], 2, initial = 10)$errors
  expect_equal(which(!is.na(errors[, 1]))[1], 11)
  expect_equal(colSums(!is.na(errors)), c(30, 29))
  errors <- fabletools:::conformal_cv_errors(fit$rw[[1]], 1, initial = 1)$errors
  expect_equal(sum(!is.na(errors)), 38)

  # Defaults to 10 observations
  cnf <- mutate(fit, rw = conformal_scp(rw))
  expect_equal(cnf$rw[[1]] %@% "initial", 10)
  fc <- forecast(mutate(fit, rw = conformal_scp(rw, n_quantiles = 50, initial = 5)), h = 2)
  expect_true(all(is.finite(fc$.mean)))

  expect_error(conformal_scp(fit$rw, initial = 0), "`initial` must be a single positive whole number")
  expect_error(conformal_scp(fit$rw, initial = 2.5), "`initial` must be a single positive whole number")
  expect_error(conformal_scp(fit$rw, initial = c(1, 2)), "`initial` must be a single positive whole number")
  expect_error(
    mutate(fit, rw = conformal_scp(rw, initial = 40)),
    "`initial` \\(40\\) must be less than the number of observations \\(40\\)"
  )
  expect_error(
    model(dt, rw = conformal_scp(fable::RW(y), initial = 50), .safely = FALSE),
    "must be less than the number of observations"
  )
  # Refitting to a shorter series is checked when forecasting
  short <- refit(cnf, dt[1:8, ])
  expect_error(forecast(short, h = 1), "must be less than the number of observations \\(8\\)")
})
