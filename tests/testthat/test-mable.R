context("test-mable.R")

test_that("Mable classes", {
  expect_s3_class(mbl, "mdl_df")
  expect_s3_class(mbl[["model"]], "lst_mdl")
})

test_that("Mable print output", {
  expect_output(print(mbl), "A mable: 1 model")
})

test_that("Mable fitted values", {
  fits <- fitted(mbl)
  expect_true(is_tsibble(fits))
  expect_true(all(colnames(fits) %in% c("index", ".fitted")))
  expect_equal(fits[["index"]], USAccDeaths[["index"]])
  expect_equal(fits[[".fitted"]], fitted(mbl[["model"]][[1]])[[".fitted"]])
  
  fits <- fitted(mbl_multi)
  expect_true(is_tsibble(fits))
  expect_equal(key_vars(fits), "key")
  expect_true(all(colnames(fits) %in% c("key", "index", ".fitted")))
  expect_equal(unique(fits[[key_vars(fits)]]), mbl_multi[[key_vars(fits)]])
  expect_equal(fits[["index"]], UKLungDeaths[["index"]])
  expect_equal(fits[[".fitted"]],
               as.numeric(c(
                 fitted(mbl_multi[["model"]][[1]])[[".fitted"]],
                 fitted(mbl_multi[["model"]][[2]])[[".fitted"]]
               ))
  )
})

test_that("Mable residuals", {
  resids <- residuals(mbl)
  expect_true(is_tsibble(resids))
  expect_true(all(colnames(resids) %in% c("index", ".resid")))
  expect_equal(resids[["index"]], USAccDeaths[["index"]])
  expect_equal(resids[[".resid"]], as.numeric(residuals(mbl[["model"]][[1]])[[".resid"]]))
  
  resids <- residuals(mbl_multi)
  expect_true(is_tsibble(resids))
  expect_equal(key_vars(resids), "key")
  expect_true(all(colnames(resids) %in% c("key", "index", ".resid")))
  expect_equal(unique(resids[[key_vars(resids)]]), mbl_multi[[key_vars(resids)]])
  expect_equal(resids[["index"]], UKLungDeaths[["index"]])
  expect_equal(resids[[".resid"]], 
               as.numeric(c(
                 residuals(mbl_multi[["model"]][[1]])[[".resid"]],
                 residuals(mbl_multi[["model"]][[2]])[[".resid"]]
               ))
  )
})