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
  expect_warning(
    fc_agg_reconciled <- fit_agg %>% reconcile(snaive = min_trace(snaive)) %>% forecast(),
    "experimental"
  )
  
  expect_equal(
    fc_agg$value,
    fc_agg_reconciled$value
  )
  expect_failure(
    expect_equal(
      fc_agg$.distribution,
      fc_agg_reconciled$.distribution
    ) 
  )
  
  fit_agg <- lung_deaths_agg %>% 
    model(ses = fable::ETS(value ~ error("A") + trend("A") + season("A")))
  fc_agg <- fit_agg %>% forecast()
  expect_warning(
    fc_agg_reconciled <- fit_agg %>% reconcile(ses = min_trace(ses)) %>% forecast(),
    "experimental"
  )
  expect_equal(
    fc_agg_reconciled$value[48 + (1:24)],
    fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)],
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  expect_warning(
    fc_agg_reconciled <- fit_agg %>% reconcile(ses = min_trace(ses, method = "wls_var")) %>% forecast(),
    "experimental"
  )
  expect_equal(
    fc_agg_reconciled$value[48 + (1:24)],
    fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)],
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  expect_warning(
    fc_agg_reconciled <- fit_agg %>% reconcile(ses = min_trace(ses, method = "ols")) %>% forecast(),
    "experimental"
  )
  expect_equal(
    fc_agg_reconciled$value[48 + (1:24)],
    fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)],
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
  
  expect_warning(
    fc_agg_reconciled <- fit_agg %>% reconcile(ses = min_trace(ses, method = "mint_cov")) %>% forecast(),
    "experimental"
  )
  expect_equal(
    fc_agg_reconciled$value[48 + (1:24)],
    fc_agg_reconciled$value[(1:24)] + fc_agg_reconciled$value[24 + (1:24)],
  )
  expect_failure(
    expect_equal(
      fc_agg$value,
      fc_agg_reconciled$value
    )
  )
})
