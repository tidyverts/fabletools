context("test-accuracy")

test_that("In-sample accuracy", {
  skip_if_not_installed("fable")
  
  expect_warning(
    accuracy(mbl, measures = interval_accuracy_measures),
    'argument ".dist" is missing'
  ) %>% 
    {is.na(.[["winkler"]])} %>%
    expect_true()
  
  acc <- accuracy(mbl)
  expect_equal(acc$.type, "Training")
  expect_equal(dim(acc), c(1,9))
  expect_true(!any(map_lgl(acc, compose(any, is.na))))
  expect_equal(
    as.list(acc),
    as_tibble(augment(mbl, type = "response")) %>% 
      group_by(.model) %>% 
      summarise(.type = "Training", ME = mean(.resid), RMSE = sqrt(mean(.resid^2)),
                MAE = mean(abs(.resid)), MPE = mean(.resid/value*100),
                MAPE = mean(abs(.resid/value)*100),
                MASE = MASE(.resid, value, .period = 12), ACF1 = ACF1(.resid)) %>% 
      as.list()
  )
  
  acc_multi <- accuracy(mbl_multi)
  expect_equal(acc_multi$key, c("fdeaths", "mdeaths"))
  expect_equal(dim(acc_multi), c(2,10))
  expect_true(!any(map_lgl(acc_multi, compose(any, is.na))))
  
  acc_complex <- accuracy(mbl_complex)
  expect_equal(acc_complex$key, rep(c("fdeaths", "mdeaths"), each = 2))
  expect_equal(acc_complex$.model, rep(c("ets", "lm"), 2))
  expect_equal(dim(acc_complex), c(4,10))
  expect_equal(acc_complex[c(1,3), -2], acc_multi[,-2])
  expect_true(!any(map_lgl(acc_complex, compose(any, is.na))))
  
  acc_mv <- accuracy(mbl_mv)
  expect_equal(
    acc_mv$.response,
    factor(c("mdeaths", "fdeaths"), levels = unique(c("mdeaths", "fdeaths")))
  )
  expect_true(!any(map_lgl(acc_mv, compose(any, is.na))))
})


test_that("Out-of-sample accuracy", {
  skip_if_not_installed("fable")
  
  expect_warning(
    accuracy(fbl, utils::head(us_deaths)),
    "12 observations are missing between 1978 Jan and 1978 Dec"
  )
  
  acc <- accuracy(fbl, us_deaths)
  expect_equal(acc$.type, "Test")
  expect_equal(dim(acc), c(1,9))
  expect_true(!any(map_lgl(acc, compose(any, is.na))))
  expect_equal(
    as.list(acc),
    as_tibble(fbl) %>% 
      mutate(
        actual = semi_join(us_deaths, fbl, by = "index")$value,
        .resid = actual - mean(value)
      ) %>% 
      group_by(.model) %>% 
      summarise(.type = "Test", ME = mean(.resid), RMSE = sqrt(mean(.resid^2)),
                MAE = mean(abs(.resid)), MPE = mean(.resid/actual*100),
                MAPE = mean(abs(.resid/actual)*100),
                MASE = MASE(.resid, us_deaths_tr$value, .period = 12), ACF1 = ACF1(.resid)) %>% 
      as.list()
  )
  
  acc <- accuracy(fbl, us_deaths, measures = list(interval_accuracy_measures, distribution_accuracy_measures))
  expect_equal(acc$.type, "Test")
  expect_equal(colnames(acc), c(".model", ".type", "winkler", "percentile", "CRPS"))
  expect_true(!any(map_lgl(acc, compose(any, is.na))))
  
  acc_multi <- accuracy(fbl_multi, lung_deaths_long)
  expect_equal(acc_multi$key, c("fdeaths", "mdeaths"))
  expect_equal(dim(acc_multi), c(2,10))
  expect_true(!any(map_lgl(acc_multi, compose(any, is.na))))
  
  acc_complex <- accuracy(fbl_complex, lung_deaths_long)
  
  expect_equal(acc_complex$key, rep(c("fdeaths", "mdeaths"), 2))
  expect_equal(acc_complex$.model, rep(c("ets", "lm"), each = 2))
  expect_equal(dim(acc_complex), c(4,10))
  expect_equal(acc_complex[1:2, -(1:2)], acc_multi[,-(1:2)])
  expect_true(!any(map_lgl(acc_complex, compose(any, is.na))))
  
  acc_mv <- accuracy(fbl_mv, lung_deaths_wide)
  expect_equal(
    acc_mv$.response,
    c("fdeaths", "mdeaths")
  )
  expect_true(!any(map_lgl(acc_mv, compose(any, is.na))))
})
