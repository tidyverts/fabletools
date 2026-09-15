context("test-stream-lag.R")

# Minimal model class using lag() (via self$recall_lag()) so tests can
# exercise recent_data snapshotting without depending on a real model
# package such as fable.
lag_test_specials <- new_specials(
  lagged = function(x, n = 1) self$recall_lag(x, n),
  xreg = function(...) NULL
)

new_lag_test_model <- function(formula, ...){
  cls <- new_model_class(
    model = "lag test model",
    train = function(.data, specials, ...){
      structure(list(x = specials$lagged[[1]]), class = "lag_test_fit")
    },
    specials = lag_test_specials
  )
  new_model_definition(cls, !!enquo(formula), ...)
}

stream.lag_test_fit <- function(object, new_data, specials, ...){
  object$x <- specials$lagged[[1]]
  object
}
registerS3method("stream", "lag_test_fit", stream.lag_test_fit)

forecast.lag_test_fit <- function(object, new_data, specials, ...){
  distributional::dist_degenerate(specials$lagged[[1]])
}
registerS3method("forecast", "lag_test_fit", forecast.lag_test_fit)

lag_test_data <- function(x, from = 1){
  tsibble::tsibble(idx = from + seq_along(x) - 1, y = x, x = x, index = idx)
}

test_that("forecast() does not leak recent_data across series sharing a model definition", {
  dt1 <- lag_test_data(c(10, 20, 30, 40, 50))
  dt2 <- lag_test_data(c(100, 200, 300, 400, 500))

  mdl <- new_lag_test_model(y ~ lagged(x))

  # Same `mdl` object reused for both series, as model.tbl_ts() does.
  fit1 <- estimate(dt1, mdl)
  fit2 <- estimate(dt2, mdl)

  new1 <- lag_test_data(60, from = 6)
  new2 <- lag_test_data(600, from = 6)

  fc1 <- forecast(fit1, new1)
  fc2 <- forecast(fit2, new2)

  # Each forecast should bridge from its own series' last x, not the other's.
  expect_equal(fc1$.mean, 50)
  expect_equal(fc2$.mean, 500)
})

test_that("stream() resolves lag() regressors correctly across chained calls", {
  dt <- lag_test_data(c(10, 20, 30, 40, 50))
  mdl <- new_lag_test_model(y ~ lagged(x))
  fit <- estimate(dt, mdl)

  new_a <- lag_test_data(c(60, 70), from = 6)
  new_b <- lag_test_data(c(80, 90), from = 8)

  chained <- fit %>% stream(new_a) %>% stream(new_b)
  one_shot <- fit %>% stream(dplyr::bind_rows(new_a, new_b))

  expect_equal(chained$fit$x, c(70, 80))
  expect_equal(tail(one_shot$fit$x, 2), c(70, 80))
})

test_that("stream() does not leak recent_data across independent calls sharing a base fit", {
  dt <- lag_test_data(c(10, 20, 30, 40, 50))
  mdl <- new_lag_test_model(y ~ lagged(x))
  fit <- estimate(dt, mdl)

  new_a <- lag_test_data(600, from = 6)
  new_b <- lag_test_data(700, from = 6)

  # Both calls reuse the original `fit`, not each other's result.
  streamed_a <- stream(fit, new_a)
  streamed_b <- stream(fit, new_b)

  expect_equal(streamed_a$fit$x, 50)
  expect_equal(streamed_b$fit$x, 50)
})
