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
