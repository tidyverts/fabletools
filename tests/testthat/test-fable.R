context("test-fable")

test_that("fable dplyr verbs", {
  skip_if_not_installed("fable")
  
  fbl_complex %>% filter(key == "mdeaths") %>% 
    expect_s3_class("fbl_ts") %>% 
    NROW %>% 
    expect_equal(24)
  
  # tsibble now automatically selects keys
  # expect_error(
  #   fbl_complex %>% select(index, .model, value, .distribution),
  #   "not a valid tsibble"
  # )
  
  fbl_complex %>%
    filter(key == "mdeaths") %>%
    select(index, .model, value, .mean) %>% 
    n_keys() %>% 
    expect_equal(2)
  
  expect_equal(
    colnames(hilo(fbl_complex, level = c(50, 80, 95))),
    c("key", ".model", "index", "value", ".mean", "50%", "80%", "95%")
  )
  
  expect_equivalent(
    as.list(fbl_multi),
    as.list(bind_rows(fbl_multi[1:12,], fbl_multi[13:24,]))
  )
})

test_that("select() and transmute() keep the distribution (#324)", {
  skip_if_not_installed("fable")

  expect_message(
    res <- select(fbl_complex, -value),
    "Selecting distribution: `value`"
  )
  expect_s3_class(res, "fbl_ts")
  expect_true("value" %in% names(res))
  expect_identical(distribution_var(res), "value")
  expect_identical(res[["value"]], fbl_complex[["value"]])

  expect_message(
    res <- select(group_by(fbl_complex, key), .mean),
    "Selecting distribution"
  )
  expect_s3_class(res, "fbl_ts")
  expect_true("value" %in% names(res))

  expect_message(
    res <- transmute(fbl_complex, m = .mean * 2),
    "Selecting distribution"
  )
  expect_s3_class(res, "fbl_ts")
  expect_true(all(c("m", "value") %in% names(res)))

  # Renaming the distribution in select()
  res <- select(fbl_complex, dist = value)
  expect_s3_class(res, "fbl_ts")
  expect_identical(distribution_var(res), "dist")
  expect_identical(response_vars(res), "value")
})

test_that("rename() and relocate() keep the fable class (#348, #403)", {
  skip_if_not_installed("fable")

  res <- relocate(fbl_complex, .mean)
  expect_s3_class(res, "fbl_ts")
  expect_identical(names(res)[1], ".mean")
  expect_identical(distribution_var(res), "value")

  res <- relocate(fbl_complex, value, .after = .mean)
  expect_s3_class(res, "fbl_ts")
  expect_identical(tail(names(res), 1), "value")

  res <- relocate(fbl_complex, dist = value)
  expect_s3_class(res, "fbl_ts")
  expect_identical(names(res)[1], "dist")
  expect_identical(distribution_var(res), "dist")

  res <- rename(fbl_complex, point = .mean)
  expect_s3_class(res, "fbl_ts")
  expect_true("point" %in% names(res))
  expect_identical(distribution_var(res), "value")

  res <- rename(fbl_complex, dist = value)
  expect_s3_class(res, "fbl_ts")
  expect_identical(distribution_var(res), "dist")
  expect_identical(response_vars(res), "value")
  expect_s3_class(res[["dist"]], "distribution")

  # Renaming keys and index is still handled by tsibble
  res <- rename(fbl_complex, series = key, time = index)
  expect_s3_class(res, "fbl_ts")
  expect_identical(tsibble::index_var(res), "time")
  expect_true("series" %in% tsibble::key_vars(res))

  # Grouped fables
  grp <- group_by(fbl_complex, key)
  res <- rename(grp, dist = value)
  expect_s3_class(res, "grouped_fbl")
  expect_identical(distribution_var(res), "dist")
  res <- relocate(grp, .mean)
  expect_s3_class(res, "grouped_fbl")
  expect_identical(names(res)[1], ".mean")
  res <- dplyr::rename_with(grp, toupper, c(value, .mean))
  expect_s3_class(res, "grouped_fbl")
  expect_identical(distribution_var(res), "VALUE")

  # rename_with() and names<-
  res <- dplyr::rename_with(fbl_complex, toupper)
  expect_s3_class(res, "fbl_df")
  expect_identical(distribution_var(res), "VALUE")
  expect_identical(response_vars(res), "value")
  res <- fbl_complex
  names(res)[names(res) == "value"] <- "dist"
  expect_s3_class(res, "fbl_df")
  expect_identical(distribution_var(res), "dist")

  # transmute() follows a renamed distribution
  res <- transmute(fbl_complex, dist = value)
  expect_identical(distribution_var(res), "dist")
  expect_false("value" %in% names(res))

  # The distribution can't be removed
  expect_error(mutate(fbl_complex, value = NULL), "must contain a distribution")
})

test_that("forecast() errors informatively when `.model` is already used (#275)", {
  skip_if_not_installed("fable")

  dt <- tsibble::tsibble(
    .model = rep(c("a", "b"), each = 10), t = rep(1:10, 2), y = 1:20,
    key = .model, index = t
  )
  expect_error(
    forecast(model(dt, naive = fable::NAIVE(y)), h = 2),
    "key variable named `.model`"
  )
  # Renaming the key avoids the clash
  expect_s3_class(
    forecast(model(dplyr::rename(dt, series = .model), naive = fable::NAIVE(y)), h = 2),
    "fbl_ts"
  )

  # A non-key `.model` column in new_data also clashes
  dt1 <- tsibble::tsibble(t = 1:10, y = 1:10, index = t)
  expect_error(
    forecast(
      model(dt1, naive = fable::NAIVE(y)),
      new_data = dplyr::mutate(tsibble::new_data(dt1, 2), .model = "x")
    ),
    "`new_data` has a column named `.model`"
  )

  # A mable fitted to components() output, which has a `.model` key
  skip_if_not_installed("feasts")
  cmp <- components(model(tsibble::as_tsibble(USAccDeaths), feasts::STL(value)))
  expect_error(
    forecast(model(cmp, fable::NAIVE(season_adjust)), h = 2),
    "key variable named `.model`"
  )
})

test_that("forecast() errors for the removed `bias_adjust` argument", {
  expect_error(forecast(mbl, h = 1, bias_adjust = TRUE), "`bias_adjust` argument")
  # Including for a single fitted model
  mdl <- mbl[[attr(mbl, "model")[1]]][[1]]
  expect_error(forecast(mdl, h = 1, bias_adjust = TRUE), "`bias_adjust` argument")
  expect_error(
    forecast(simulate_iid(mdl, times = 2), h = 1, bias_adjust = TRUE),
    "`bias_adjust` argument"
  )
})

test_that("rbind() of fables is defunct in favour of bind_rows()", {
  skip_if_not_installed("fable")
  expect_error(rbind(fbl, fbl), class = "defunctError")
})

test_that("forecast() of models gives their forecast distributions", {
  skip_if_not_installed("fable")
  mdls <- mbl[[attr(mbl, "model")[1]]]
  fc <- forecast(mdls[[1]], h = 3)
  expect_s3_class(fc, "distribution")
  expect_length(fc, 3)
  expect_identical(dist_variates(fc), response_vars(mbl))

  # A list of models gives a list of their distributions
  fc_lst <- forecast(mdls, h = 3)
  expect_type(fc_lst, "list")
  expect_length(fc_lst, length(mdls))
  expect_equal(fc_lst[[1]], fc)
  expect_equal(forecast(mbl, h = 3)[[response_vars(mbl)]], vec_c(!!!fc_lst))
})

forecast.legacy_fc_mdl <- function(object, new_data = NULL, h = NULL,
                                   point_forecast = list(.mean = mean), ...){
  legacy_point_forecast <<- point_forecast
  new_data <- new_data %||% make_future_data(object$data, h)
  resp <- response_vars(object)
  new_data[[resp]] <- NextMethod()
  new_data[names(point_forecast)] <- compute_point_forecasts(new_data[[resp]], point_forecast)
  build_fable(new_data, response = resp, distribution = resp)
}
registerS3method("forecast", "legacy_fc_mdl", forecast.legacy_fc_mdl)

test_that("forecasts returned as a fable by model methods are converted to distributions", {
  skip_if_not_installed("fable")
  nm <- attr(mbl, "model")[1]
  mdls <- mbl[[nm]]
  legacy <- map_models(mdls, function(m) structure(m, class = c("legacy_fc_mdl", class(m))))

  # Directly from a list of models, and with the point forecasts of a mable
  expect_equal(forecast(legacy, h = 3), forecast(mdls, h = 3))
  fc <- forecast(dplyr::mutate(mbl, !!nm := legacy), h = 3, point_forecast = list(.median = median))
  expect_equal(fc, forecast(mbl, h = 3, point_forecast = list(.median = median)))
  # Point forecasts aren't computed by the models of a mable
  expect_identical(legacy_point_forecast, list())
})
