context("test-multivariate.R")

test_that("multiple univariate", {
  fit <- UKLungDeaths %>%
    fable::ETS(value)
  
  expect_equal(sort(fit$key), c("fdeaths", "mdeaths"))
  expect_s3_class(fit$model, "lst_mdl")
  fc <- fit %>% forecast
  
  expect_equal(sort(fc$key), c("fdeaths", "mdeaths"))
  expect_s3_class(fc$forecast, "lst_fc")
})
