context("test-bootstrap")

test_that("bootstrap_iid / bootstrap_block / simulate_iid", {
  skip_if_not_installed("fable")

  set.seed(1)
  fc_bi <- mbl %>%
    mutate(ets = bootstrap_iid(ets, times = 100)) %>%
    forecast(h = 12)
  expect_true(all(dist_types(fc_bi$value) == "dist_sample"))
  expect_equal(unique(lengths(distributional::parameters(fc_bi$value)$x)), 100)

  fc_bb <- mbl %>%
    mutate(ets = bootstrap_block(ets, times = 100)) %>%
    forecast(h = 12)
  expect_true(all(dist_types(fc_bb$value) == "dist_sample"))
  expect_equal(unique(lengths(distributional::parameters(fc_bb$value)$x)), 100)

  fc_sp <- mbl %>%
    mutate(ets = simulate_iid(ets, times = 100)) %>%
    forecast(h = 12)
  expect_true(all(dist_types(fc_sp$value) == "dist_sample"))
  expect_equal(unique(lengths(distributional::parameters(fc_sp$value)$x)), 100)

  # Bootstrap distribution differs from analytical, but means are similar.
  expect_false(identical(fc_bi$value, fbl$value))
  expect_equal(mean(fc_bi$.mean), mean(fbl$.mean), tolerance = 0.1)
})

test_that("bootstrap_* modifiers work on mdl_lst columns (multiple series/models)", {
  skip_if_not_installed("fable")

  fc_multi <- mbl_multi %>%
    mutate(ets = bootstrap_iid(ets, times = 50)) %>%
    forecast(h = 12)
  expect_true(all(dist_types(fc_multi$value) == "dist_sample"))
  expect_equal(n_keys(fc_multi), 2)

  # Each model column is modified independently.
  fc_mix <- mbl_complex %>%
    mutate(
      ets = bootstrap_iid(ets, times = 50),
      lm = simulate_iid(lm, times = 50)
    ) %>%
    forecast(h = 12)
  expect_true(all(dist_types(fc_mix$value) == "dist_sample"))
})

test_that("bootstrapped forecasts reconcile jointly (dist_sample branch)", {
  skip_if_not_installed("fable")

  lung_deaths_agg <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value))

  fit_agg <- lung_deaths_agg %>%
    model(snaive = fable::SNAIVE(value))

  reconciled <- fit_agg %>%
    mutate(snaive = min_trace(bootstrap_iid(snaive, times = 100)))

  # min_trace() must preserve the mdl_lst_bootstrap_iid class so
  # forecast.lst_mint_mdl()'s NextMethod() reaches the joint bootstrap
  # forecast method instead of independent per-series sampling.
  expect_true(inherits(reconciled$snaive, "mdl_lst_bootstrap_iid"))

  fc_agg <- reconciled %>% forecast()

  expect_true(all(dist_types(fc_agg$value) == "dist_sample"))

  tot <- fc_agg %>% dplyr::filter(is_aggregated(key)) %>% dplyr::pull(.mean)
  comp <- fc_agg %>%
    dplyr::filter(!is_aggregated(key)) %>%
    as_tibble() %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(s = sum(.mean)) %>%
    dplyr::pull(s)
  expect_equal(tot, comp)
})

test_that("bootstrap_iid/bootstrap_block sample jointly across series in a mdl_lst", {
  skip_if_not_installed("fable")

  # Identical series share identical residuals, so a joint bootstrap draw
  # gives identical paths; independent draws would (almost surely) differ.
  two_id <- dplyr::bind_rows(
    dplyr::mutate(as_tibble(us_deaths_tr), id = "A"),
    dplyr::mutate(as_tibble(us_deaths_tr), id = "B")
  ) %>% as_tsibble(index = index, key = id)
  mbl_two_id <- two_id %>% model(ets = fable::ETS(value))

  set.seed(1)
  fc_joint <- mbl_two_id %>%
    mutate(ets = bootstrap_iid(ets, times = 20)) %>%
    forecast(h = 6)
  paths_a <- distributional::parameters(dplyr::filter(fc_joint, id == "A")$value)$x
  paths_b <- distributional::parameters(dplyr::filter(fc_joint, id == "B")$value)$x
  expect_true(all(mapply(identical, paths_a, paths_b)))

  set.seed(1)
  fc_joint_block <- mbl_two_id %>%
    mutate(ets = bootstrap_block(ets, times = 20)) %>%
    forecast(h = 6)
  paths_a <- distributional::parameters(dplyr::filter(fc_joint_block, id == "A")$value)$x
  paths_b <- distributional::parameters(dplyr::filter(fc_joint_block, id == "B")$value)$x
  expect_true(all(mapply(identical, paths_a, paths_b)))

  # Contrast: independent bootstrapping should not give identical paths.
  set.seed(1)
  fc_a <- forecast(bootstrap_iid(mbl_two_id$ets[[1]], times = 20), h = 6)
  fc_b <- forecast(bootstrap_iid(mbl_two_id$ets[[2]], times = 20), h = 6)
  paths_a <- distributional::parameters(fc_a$value)$x
  paths_b <- distributional::parameters(fc_b$value)$x
  expect_false(all(mapply(identical, paths_a, paths_b)))
})

test_that("joint bootstrap warns and restricts to the overlapping time domain", {
  skip_if_not_installed("fable")

  two_id_trunc <- dplyr::bind_rows(
    dplyr::mutate(as_tibble(us_deaths_tr), id = "A"),
    dplyr::mutate(as_tibble(dplyr::filter(us_deaths_tr, index < tsibble::yearmonth("1977 Jul"))), id = "B")
  ) %>% as_tsibble(index = index, key = id)
  mbl_trunc <- two_id_trunc %>% model(ets = fable::ETS(value))

  expect_warning(
    fc <- mbl_trunc %>% mutate(ets = bootstrap_iid(ets, times = 20)) %>% forecast(h = 6),
    "overlapping period"
  )
  expect_equal(nrow(fc), 12)
})

test_that("bootstrap_iid/bootstrap_block/simulate_iid apply to a model specification, fitting each series independently", {
  skip_if_not_installed("fable")

  fit_bi <- lung_deaths_long_tr %>%
    model(ets = bootstrap_iid(fable::ETS(value), times = 20))
  expect_true(inherits(fit_bi$ets[[1]], "mdl_bootstrap_iid"))

  set.seed(1)
  fc_bi <- fit_bi %>% forecast(h = 6)
  expect_true(all(dist_types(fc_bi$value) == "dist_sample"))

  # Applied to the spec, each series is bootstrapped independently, unlike
  # wrapping a fitted mdl_lst column (jointly, see tests above).
  paths_a <- distributional::parameters(dplyr::filter(fc_bi, key == "mdeaths")$value)$x
  paths_b <- distributional::parameters(dplyr::filter(fc_bi, key == "fdeaths")$value)$x
  expect_false(all(mapply(identical, paths_a, paths_b)))

  fit_bb <- lung_deaths_long_tr %>%
    model(ets = bootstrap_block(fable::ETS(value), times = 20))
  expect_true(inherits(fit_bb$ets[[1]], "mdl_bootstrap_block"))
  fc_bb <- fit_bb %>% forecast(h = 6)
  expect_true(all(dist_types(fc_bb$value) == "dist_sample"))

  fit_si <- lung_deaths_long_tr %>%
    model(ets = simulate_iid(fable::ETS(value), times = 20))
  expect_true(inherits(fit_si$ets[[1]], "mdl_ts_sim"))
  fc_si <- fit_si %>% forecast(h = 6)
  expect_true(all(dist_types(fc_si$value) == "dist_sample"))

  # An unwrapped spec still fits and forecasts normally.
  fit_plain <- lung_deaths_long_tr %>% model(ets = fable::ETS(value))
  expect_false(inherits(fit_plain$ets[[1]], "mdl_ts_sim"))
})

test_that("deprecated forecast()/generate() bootstrap/simulate arguments still work", {
  skip_if_not_installed("fable")

  expect_warning(
    fc <- mbl %>% forecast(h = 12, bootstrap = TRUE, times = 50),
    "deprecated"
  )
  expect_true(all(dist_types(fc$value) == "dist_sample"))

  expect_warning(
    fc <- mbl %>% forecast(h = 12, simulate = TRUE, times = 50),
    "deprecated"
  )
  expect_true(all(dist_types(fc$value) == "dist_sample"))

  expect_warning(
    gen <- generate(mbl$ets[[1]], h = 12, times = 3, bootstrap = TRUE),
    "deprecated"
  )
  expect_equal(NROW(gen), 12 * 3)
})
