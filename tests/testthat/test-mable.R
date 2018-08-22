context("test-mable.R")

test_that("Mable classes", {
  expect_s3_class(mbl, c("mable", "lst_ts"))
  expect_s3_class(mbl[["model"]], "lst_mdl")
})

test_that("Mable print output", {
  expect_output(print(mbl), "A mable: 1 model \\[1M\\]")
})

test_that("Mable fitted values", {
  fits <- fitted(mbl)
  expect_true(is_tsibble(fits))
  expect_equal(colnames(fits), c("index", "fitted"))
  expect_equal(fits[["index"]], USAccDeaths[["index"]])
  expect_equal(fits[["fitted"]], as.numeric(fitted(mbl[["model"]][[1]])))
  
  fits <- fitted(mbl_multi)
  expect_true(is_tsibble(fits))
  expect_equal(key_vars(fits), "key")
  expect_equal(colnames(fits), c("key", "index", "fitted"))
  # expect_equal(unique(fits[[key_vars(fits)]]), mbl_multi[[key_vars(fits)]])
  expect_equal(fits[["index"]], UKLungDeaths[["index"]])
  # https://github.com/tidyverts/tsibble/issues/57
  # expect_equal(fits[["fitted"]], as.numeric(c(fitted(mbl_multi[["model"]][[1]]),fitted(mbl_multi[["model"]][[2]]))))
})

test_that("Mable residuals", {
  resids <- residuals(mbl)
  expect_true(is_tsibble(resids))
  expect_equal(colnames(resids), c("index", "residuals"))
  expect_equal(resids[["index"]], USAccDeaths[["index"]])
  expect_equal(resids[["residuals"]], as.numeric(residuals(mbl[["model"]][[1]])))
  
  resids <- residuals(mbl_multi)
  expect_true(is_tsibble(resids))
  expect_equal(key_vars(resids), "key")
  expect_equal(colnames(resids), c("key", "index", "residuals"))
  # expect_equal(unique(resids[[key_vars(resids)]]), mbl_multi[[key_vars(resids)]])
  expect_equal(resids[["index"]], UKLungDeaths[["index"]])
  # https://github.com/tidyverts/tsibble/issues/57
  # expect_equal(resids[["residuals"]], as.numeric(c(residuals(mbl_multi[["model"]][[1]]),residuals(mbl_multi[["model"]][[2]]))))
})