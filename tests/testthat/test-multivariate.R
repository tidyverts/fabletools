context("test-multivariate.R")

test_that("multiple univariate", {
  expect_equal(sort(mbl_multi$key), c("fdeaths", "mdeaths"))
  expect_s3_class(mbl_multi$model, "lst_mdl")
  
  expect_equal(sort(fbl_multi$key), c("fdeaths", "mdeaths"))
  expect_s3_class(fbl_multi$forecast, "lst_fc")
})
