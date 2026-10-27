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

test_that("min_trace accepts series with very different scales", {
  skip_if_not_installed("fable")

  # Series standard deviations differ by a factor of about 1e5
  lung_deaths_scaled <- lung_deaths_long %>%
    dplyr::mutate(value = ifelse(key == "fdeaths", value / 1e5, value)) %>%
    aggregate_key(key, value = sum(value))
  fit <- lung_deaths_scaled %>%
    model(ets = fable::ETS(value ~ error("A") + trend("N") + season("A")))

  for (method in c("ols", "wls_var", "wls_struct", "mint_cov", "mint_shrink")) {
    expect_no_error(
      fc <- fit %>%
        mutate(ets = reconcile_mint(ets, method = !!method)) %>%
        forecast(h = 6)
    )
    fc <- tidyr::pivot_wider(
      tibble::as_tibble(fc)[c("key", "index", ".mean")],
      names_from = key, values_from = .mean
    )
    expect_equal(fc[["<aggregated>"]], fc[["mdeaths"]] + fc[["fdeaths"]])
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

test_that("top_down and middle_out reconcile sample paths (#408)", {
  skip_if_not_installed("fable")

  set.seed(408)
  sim_hts <- tidyr::expand_grid(
    index    = tsibble::yearmonth("2020 Jan") + 0:35,
    category = c("Food", "Tech"),
    sku      = c("S1", "S2", "S3")
  ) |>
    dplyr::mutate(sales = rpois(dplyr::n(), lambda = 50)) |>
    tsibble::as_tsibble(key = c(category, sku), index = index) |>
    aggregate_key(category / sku, sales = sum(sales))

  fc_tbl <- sim_hts |>
    model(snaive = fable::SNAIVE(sales)) |>
    mutate(snaive = simulate_iid(snaive, times = 100)) |>
    mutate(td = reconcile_td(snaive), mo = reconcile_mo(snaive)) |>
    forecast(h = 3) |>
    dplyr::filter(.model != "snaive") |>
    as_tibble()

  # Sample paths are kept rather than collapsed to degenerate distributions
  expect_true(all(distributional::variance(fc_tbl$sales) > 0))

  # Each reconciled sample path is coherent
  sample_paths <- function(x) do.call(cbind, distributional::parameters(x)$x)
  for (mdl in c("td", "mo")) {
    fc <- dplyr::filter(fc_tbl, .model == mdl)
    total <- dplyr::filter(fc, is_aggregated(category)) |> dplyr::arrange(index)
    btm <- dplyr::filter(fc, !is_aggregated(sku)) |> dplyr::arrange(index)
    btm_sum <- Reduce(`+`, lapply(
      split(btm$sales, interaction(btm$category, btm$sku)), sample_paths
    ))
    expect_equal(sample_paths(total$sales), btm_sum)
    expect_equal(total$.mean, colMeans(btm_sum))
  }
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

test_that("top_down and middle_out reconcile forecasts using new_data", {
  skip_if_not_installed("fable")

  set.seed(382)
  sim_hts <- tidyr::expand_grid(
    index    = tsibble::yearmonth("2020 Jan") + 0:23,
    category = c("A", "B"),
    sku      = c("S1", "S2")
  ) |>
    dplyr::mutate(x = rnorm(dplyr::n()), sales = 50 + 3 * x + rnorm(dplyr::n())) |>
    tsibble::as_tsibble(key = c(category, sku), index = index) |>
    aggregate_key(category / sku, sales = sum(sales), x = sum(x))
  future <- tsibble::new_data(sim_hts, 3) |>
    dplyr::mutate(x = seq_len(dplyr::n()))

  fc <- sim_hts |>
    model(lm = fable::TSLM(sales ~ x)) |>
    mutate(
      td_avg = reconcile_td(lm, method = "average_proportions"),
      td_prop = reconcile_td(lm, method = "proportion_averages"),
      mo = reconcile_mo(lm, split = 1)
    ) |>
    forecast(new_data = future) |>
    as_tibble()
  base <- dplyr::filter(fc, .model == "lm")

  # The top level is forecast with its own new_data
  base_top <- dplyr::filter(base, is_aggregated(category))$.mean
  for (mdl in c("td_avg", "td_prop")) {
    expect_equal(dplyr::filter(fc, .model == mdl, is_aggregated(category))$.mean, base_top)
  }

  # The split level is forecast with its own new_data
  split_level <- function(x) dplyr::filter(x, !is_aggregated(category), is_aggregated(sku))
  expect_equal(split_level(dplyr::filter(fc, .model == "mo"))$.mean, split_level(base)$.mean)
  expect_equal(
    dplyr::filter(fc, .model == "mo", is_aggregated(category))$.mean,
    as.numeric(tapply(split_level(base)$.mean, split_level(base)$index, sum))
  )
})

test_that("top_down and middle_out disaggregate the distributions of their top levels", {
  skip_if_not_installed("fable")

  set.seed(1408)
  sim_hts <- tidyr::expand_grid(
    index    = tsibble::yearmonth("2020 Jan") + 0:35,
    category = c("Food", "Tech"),
    sku      = c("S1", "S2")
  ) |>
    dplyr::mutate(sales = rpois(dplyr::n(), lambda = 50)) |>
    tsibble::as_tsibble(key = c(category, sku), index = index) |>
    aggregate_key(category / sku, sales = sum(sales))
  fit <- model(sim_hts, snaive = fable::SNAIVE(sales))

  # Sample paths below the top level don't change normal top level forecasts
  is_top <- is_aggregated(fit$category)
  fit$snaive[!is_top] <- simulate_iid(fit$snaive[!is_top], times = 10)
  fc <- fit |>
    mutate(td = reconcile_td(snaive)) |>
    forecast(h = 2)
  td <- dplyr::filter(fc, .model == "td")
  expect_true(all(dist_types(td$sales) == "dist_normal"))
  expect_true(all(distributional::variance(td$sales) > 0))

  # Splitting middle_out at the second level uses the second level distributions
  fit <- model(sim_hts, snaive = fable::SNAIVE(sales))
  is_mid <- !is_aggregated(fit$category) & is_aggregated(fit$sku)
  fit$snaive[!is_mid] <- simulate_iid(fit$snaive[!is_mid], times = 10)
  mo <- fit |>
    mutate(mo = reconcile_mo(snaive, split = 1)) |>
    forecast(h = 2) |>
    dplyr::filter(.model == "mo")
  expect_true(all(dist_types(mo$sales) == "dist_normal"))
  expect_true(all(distributional::variance(mo$sales) > 0))
})

test_that("middle_out splitting at the bottom level is bottom up reconciliation", {
  skip_if_not_installed("fable")

  lung_deaths_agg <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value))
  fc <- lung_deaths_agg %>%
    model(snaive = fable::SNAIVE(value)) %>%
    mutate(mo = reconcile_mo(snaive), bu = reconcile_bu(snaive)) %>%
    forecast(h = 3)
  expect_equal(
    dplyr::filter(fc, .model == "mo")$value,
    dplyr::filter(fc, .model == "bu")$value
  )
})
