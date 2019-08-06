context("test-multivariate.R")

test_that("multiple univariate", {
  skip_if_not_installed("fable")
  
  expect_equal(sort(mbl_multi$key), c("fdeaths", "mdeaths"))
  expect_s3_class(mbl_multi[[attr(mbl_multi,"models")[[1]]]], "lst_mdl")
  
  expect_equal(sort(unique(fbl_multi$key)), c("fdeaths", "mdeaths"))
  expect_s3_class(fbl_multi[[attr(fbl_multi,"dist")]], "fcdist")
})
