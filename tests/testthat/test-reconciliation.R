context("test-reconciliation")

test_that("reconciliation", {
  lung_deaths_agg <- lung_deaths_long %>% 
    aggregate_key(key, value = sum(value))
  expect_equal(n_keys(lung_deaths_agg), 3)
  expect_equal(
    lung_deaths_agg$value[1:72], 
    lung_deaths_long$value[1:72] + lung_deaths_long$value[72 + (1:72)]
  )
  expect_output(
    print(lung_deaths_agg$key),
    "<aggregated>"
  )
  expect_output(
    print(lung_deaths_agg),
    "<aggregated>"
  )
  
  skip_if_not_installed("fable")
  
  fit_agg <- lung_deaths_agg %>% 
    model(snaive = fable::SNAIVE(value))
  
  fc_agg <- fit_agg %>% forecast()
  fc_agg_reconciled <- fit_agg %>% mutate(snaive = reconcile_mint(snaive)) %>% forecast()
  
  expect_equal(
    mean(fc_agg$value),
    mean(fc_agg_reconciled$value)
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    ) 
  )
  
  fit_agg <- lung_deaths_agg %>% 
    model(ses = fable::ETS(value ~ error("A") + trend("A") + season("A")))
  fc_agg <- fit_agg %>% forecast()
  fc_agg_reconciled <- fit_agg %>% mutate(ses = reconcile_mint(ses)) %>% forecast()
  expect_equal(
    mean(fc_agg_reconciled$value[48 + (1:24)]),
    mean(fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)]),
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  fc_agg_reconciled <- fit_agg %>% mutate(ses = reconcile_mint(ses, method = "wls_var")) %>% forecast()
  expect_equal(
    mean(fc_agg_reconciled$value[48 + (1:24)]),
    mean(fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)])
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  fc_agg_reconciled <- fit_agg %>% mutate(ses = reconcile_mint(ses, method = "ols")) %>% forecast()
  expect_equal(
    mean(fc_agg_reconciled$value[48 + (1:24)]),
    mean(fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)])
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  fc_agg_reconciled <- fit_agg %>% mutate(ses = reconcile_mint(ses, method = "mint_cov")) %>% forecast()
  expect_equal(
    mean(fc_agg_reconciled$value[48 + (1:24)]),
    mean(fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)])
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
})

test_that("reconcile() is deprecated", {
  skip_if_not_installed("fable")

  fit <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value)) %>%
    model(snaive = fable::SNAIVE(value))
  lifecycle::expect_deprecated(
    fc <- fit %>% reconcile(snaive = min_trace(snaive)) %>% forecast()
  )
  expect_equal(
    fc,
    fit %>% mutate(snaive = reconcile_mint(snaive)) %>% forecast()
  )
})

test_that("min_trace positive definite check is scale invariant (#358)", {
  skip_if_not_installed("fable")

  lung_deaths_agg <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value))
  # Residual variances of the rescaled data are well below the previous
  # absolute tolerance of 1e-8.
  lung_deaths_small <- lung_deaths_agg %>%
    dplyr::mutate(value = value / 1e7)

  fit <- lung_deaths_agg %>%
    model(lm = fable::TSLM(value ~ trend() + season()))
  fit_small <- lung_deaths_small %>%
    model(lm = fable::TSLM(value ~ trend() + season()))

  # mint_cov is excluded as TSLM residuals are exactly additive across the
  # hierarchy, giving a truly singular sample covariance matrix.
  for (method in c("wls_var", "mint_shrink")) {
    fc <- fit %>%
      mutate(lm = reconcile_mint(lm, method = !!method)) %>%
      forecast(h = 6)
    expect_no_error(
      fc_small <- fit_small %>%
        mutate(lm = reconcile_mint(lm, method = !!method)) %>%
        forecast(h = 6)
    )
    # Rescaling the data should rescale the reconciled forecasts
    expect_equal(fc_small$.mean, fc$.mean / 1e7)
  }
})

test_that("min_trace errors informatively for singular covariance matrices", {
  skip_if_not_installed("fable")

  # A parent with a single child produces identical series in the hierarchy
  single_child <- lung_deaths_long %>%
    dplyr::filter(key == "mdeaths") %>%
    aggregate_key(key, value = sum(value))

  fit <- single_child %>%
    model(snaive = fable::SNAIVE(value))
  expect_error(
    fit %>%
      mutate(snaive = reconcile_mint(snaive, method = "mint_cov")) %>%
      forecast(h = 6),
    "singular"
  )
  expect_no_error(
    fit %>%
      mutate(snaive = reconcile_mint(snaive, method = "wls_struct")) %>%
      forecast(h = 6)
  )
})

test_that("top_down reconciles multi-level hierarchies", {
  skip_if_not_installed("fable")

  # 4-level hierarchy: region (2) x product (3) x channel (2) = 12 bottom series
  set.seed(7213)
  sim_hts <- tidyr::expand_grid(
    index   = tsibble::yearmonth("2020 Jan") + 0:35,
    region  = c("North", "South"),
    product = c("A", "B", "C"),
    channel = c("Online", "Offline")
  ) |>
    dplyr::mutate(sales = rpois(dplyr::n(), lambda = 50)) |>
    tsibble::as_tsibble(key = c(region, product, channel), index = index) |>
    aggregate_key(region / product / channel, sales = sum(sales))

  fc_tbl <- sim_hts |>
    model(snaive = fable::SNAIVE(sales)) |>
    mutate(td = reconcile_td(snaive, method = "forecast_proportions")) |>
    forecast(h = 3) |>
    dplyr::filter(.model == "td") |>
    as_tibble()

  total <- fc_tbl |>
    dplyr::filter(is_aggregated(region), is_aggregated(product), is_aggregated(channel)) |>
    dplyr::arrange(index)

  # Total must equal sum of first level below
  region_sum <- fc_tbl |>
    dplyr::filter(!is_aggregated(region), is_aggregated(product), is_aggregated(channel)) |>
    dplyr::summarise(.mean = sum(.mean), .by = index) |>
    dplyr::arrange(index)
  expect_equal(total$.mean, region_sum$.mean)

  # Total must equal sum of bottom level
  btm_sum <- fc_tbl |>
    dplyr::filter(!is_aggregated(channel)) |>
    dplyr::summarise(.mean = sum(.mean), .by = index) |>
    dplyr::arrange(index)
  expect_equal(total$.mean, btm_sum$.mean)
})

test_that("middle_out reconciles multi-level hierarchies", {
  skip_if_not_installed("fable")

  # 4-level hierarchy: category (3) x segment (2) x sku (3) = 18 bottom series
  set.seed(4871)
  sim_hts <- tidyr::expand_grid(
    index    = tsibble::yearmonth("2020 Jan") + 0:35,
    category = c("Food", "Tech", "Apparel"),
    segment  = c("Premium", "Budget"),
    sku      = c("S1", "S2", "S3")
  ) |>
    dplyr::mutate(sales = rpois(dplyr::n(), lambda = 50)) |>
    tsibble::as_tsibble(key = c(category, segment, sku), index = index) |>
    aggregate_key(category / segment / sku, sales = sum(sales))

  fc_tbl <- sim_hts |>
    model(snaive = fable::SNAIVE(sales)) |>
    mutate(mo = reconcile_mo(snaive, split = 1)) |>
    forecast(h = 3) |>
    dplyr::filter(.model == "mo") |>
    as_tibble()

  # Split-level (category) nodes must equal sum of their bottom-level descendants
  cat_agg <- fc_tbl |>
    dplyr::filter(!is_aggregated(category), is_aggregated(segment), is_aggregated(sku)) |>
    dplyr::arrange(index, category)
  cat_from_btm <- fc_tbl |>
    dplyr::filter(!is_aggregated(sku)) |>
    dplyr::summarise(.mean = sum(.mean), .by = c(index, category)) |>
    dplyr::arrange(index, category)
  expect_equal(cat_agg$.mean, cat_from_btm$.mean)

  # Total must equal sum of split level
  total <- fc_tbl |>
    dplyr::filter(is_aggregated(category), is_aggregated(segment), is_aggregated(sku)) |>
    dplyr::arrange(index)
  total_from_cat <- fc_tbl |>
    dplyr::filter(!is_aggregated(category), is_aggregated(segment), is_aggregated(sku)) |>
    dplyr::summarise(.mean = sum(.mean), .by = index) |>
    dplyr::arrange(index)
  expect_equal(total$.mean, total_from_cat$.mean)
})

test_that("reconcile_*() aliases match their underlying functions", {
  skip_if_not_installed("fable")

  lung_deaths_agg <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value))
  fit_agg <- lung_deaths_agg %>%
    model(snaive = fable::SNAIVE(value))
  snaive <- fit_agg$snaive

  # Each alias is a thin wrapper, so it should produce an identical wrapped
  # object (same classes/attributes) to the function it aliases.
  expect_equal(reconcile_mint(snaive), min_trace(snaive))
  expect_equal(reconcile_mint(snaive, method = "ols"), min_trace(snaive, method = "ols"))
  expect_equal(reconcile_bu(snaive), bottom_up(snaive))
  expect_equal(reconcile_td(snaive), top_down(snaive))
  expect_equal(reconcile_mo(snaive), middle_out(snaive))
  expect_equal(reconcile_mo(snaive, split = 2), middle_out(snaive, split = 2))
})
