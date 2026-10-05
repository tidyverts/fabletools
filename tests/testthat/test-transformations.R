context("test-transformations")

simple_data <- tsibble(idx = 1:10, y = abs(rnorm(10)), x = 1:10, index = idx)
test_transformation <- function(..., dt = simple_data){
  mdl <- estimate(dt, no_specials(...))
  trans <- mdl$transformation[[1]]
  resp <- mdl$response[[1]]
  expect_equal(
    dt[[expr_name(resp)]],
    fabletools:::invert_transformation(trans)(trans(dt[[expr_name(resp)]]))
  )
}

test_that("single transformations", {
  test_transformation(y)
  test_transformation(y + 10)
  test_transformation(10 + y)
  test_transformation(+y)
  test_transformation(y - 10)
  test_transformation(10 - y)
  test_transformation(-y)
  test_transformation(3*y)
  test_transformation(y*3)
  test_transformation(3/y)
  test_transformation(y/3)
  test_transformation(log(y))
  test_transformation(logb(y, 10))
  test_transformation(log10(y))
  test_transformation(log2(y))
  test_transformation(log1p(y))
  test_transformation(expm1(y))
  test_transformation(exp(y))
  test_transformation(box_cox(y, 0.4))
  test_transformation(inv_box_cox(y, 0.4))
  test_transformation(sqrt(y))
  test_transformation(y^2)
  test_transformation(2^y)
  test_transformation((y))
})


test_that("transformation chains", {
  test_transformation(y + 10 - 10)
  test_transformation(10 + y * 10)
  test_transformation(+y - y)
  test_transformation(y^2 + 3)
  test_transformation(log(sqrt(y)))
  test_transformation(log(y + 1))
  test_transformation(box_cox(y^2,0.3))
  test_transformation(box_cox(y,0.3) + 1)
  
  # Something too complex
  expect_error(
    test_transformation(box_cox(y,0.3)^2),
    "Could not identify a valid back-transformation"
  )
  
  # Something rediculous
  test_transformation(log(sqrt(sqrt(sqrt(sqrt(sqrt(y)))+3))))
})

test_that("time-varying transformation parameters (#382)", {
  skip_if_not_installed("fable")
  dt <- lung_deaths_long_tr %>% 
    dplyr::mutate(lambda = ifelse(key == "mdeaths", 0.3, 0.2))
  new_dt <- new_data(dt, 12) %>% 
    dplyr::mutate(lambda = ifelse(key == "mdeaths", 0.3, 0.2))
  
  # resp() identifies the response, `lambda` is a time-varying parameter
  mdl <- model(dt, ets = fable::ETS(box_cox(resp(value), lambda)))
  expect_equal(response_vars(mdl), "value")
  resp <- response(mdl)
  expect_equal(
    resp$.response,
    dplyr::left_join(resp, dt, by = c("key", "index"))$value
  )
  fits <- fitted(mdl)
  expect_true(all(is.finite(fits$.fitted)))
  expect_true(all(fits$.fitted > 100))
  fc <- forecast(mdl, new_data = new_dt)
  expect_true(all(fc$.mean > 100))
  expect_error(
    forecast(mdl, h = 12),
    "time-varying parameter"
  )
  expect_error(
    generate(mdl, h = 12),
    "time-varying parameter"
  )
  expect_true(all(generate(mdl, new_data = new_dt)$.sim > 0))

  # Parameters are stored alongside the response
  mdl_ts <- mdl$ets[[1]]
  expect_equal(fabletools:::model_response_cols(mdl_ts), "box_cox(value, lambda)")
  expect_true("lambda" %in% names(mdl_ts$data))

  # Multi-step fitted values refit and forecast with the stored parameters
  fits_h2 <- fitted(mdl[1,], h = 2)
  expect_true(all(fits_h2$.fitted[-(1:2)] > 100, na.rm = TRUE))

  # refit() and stream() use parameters from new_data
  full <- lung_deaths_long %>%
    dplyr::mutate(lambda = ifelse(key == "mdeaths", 0.3, 0.2))
  mdl_refit <- refit(mdl, full)
  expect_equal(nrow(mdl_refit$ets[[1]]$data), nrow(dplyr::filter(full, key == "fdeaths")))
  expect_true(all(fitted(mdl_refit)$.fitted > 100))
  new_obs <- dplyr::filter(full, index >= tsibble::yearmonth("1979 Jan"))
  mdl_stream <- stream(mdl, new_obs)
  expect_equal(mdl_stream$ets[[1]]$data$lambda, rep(0.2, 72))
  expect_error(stream(mdl, dplyr::select(new_obs, -lambda)), "time-varying parameter")

  # Equivalent to a length-1 (time invariant) parameter
  mdl_const <- model(dt, ets = fable::ETS(box_cox(value, dplyr::first(lambda))))
  expect_equal(response_vars(mdl_const), "value")
  expect_equal(fitted(mdl_const)$.fitted, fits$.fitted)
  expect_equal(
    forecast(mdl_const, h = 12)$.mean,
    fc$.mean
  )
})
