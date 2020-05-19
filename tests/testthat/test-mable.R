context("test-mable.R")

test_that("Mable classes", {
  skip_if_not_installed("fable")
  expect_s3_class(mbl, "mdl_df")
  expect_s3_class(mbl[[attr(mbl,"model")[[1]]]], "lst_mdl")
})

test_that("Mable print output", {
  skip_if_not_installed("fable")
  expect_output(print(mbl), "A mable:")
})

test_that("Mable fitted values", {
  skip_if_not_installed("fable")
  fits <- fitted(mbl)
  expect_true(is_tsibble(fits))
  expect_true(all(colnames(fits) %in% c(".model", "index", ".fitted")))
  expect_equal(fits[["index"]], us_deaths_tr[["index"]])
  expect_equal(
    fits[[".fitted"]],
    fitted(mbl[[attr(mbl,"model")[[1]]]][[1]])[[".fitted"]]
  )
  
  fits <- fitted(mbl_multi)
  expect_true(is_tsibble(fits))
  expect_equal(key_vars(fits), c("key", ".model"))
  expect_true(all(colnames(fits) %in% c("key", ".model", "index", ".fitted")))
  expect_equal(unique(fits[["key"]]), mbl_multi[["key"]])
  expect_equal(fits[["index"]], lung_deaths_long_tr[["index"]])
  expect_equal(fits[[".fitted"]],
               as.numeric(c(
                 fitted(mbl_multi[[attr(mbl,"model")[[1]]]][[1]])[[".fitted"]],
                 fitted(mbl_multi[[attr(mbl,"model")[[1]]]][[2]])[[".fitted"]]
               ))
  )
})

test_that("Mable residuals", {
  skip_if_not_installed("fable")
  resids <- residuals(mbl)
  expect_true(is_tsibble(resids))
  expect_true(all(colnames(resids) %in% c(".model", "index", ".resid")))
  expect_equal(resids[["index"]], us_deaths_tr[["index"]])
  expect_equal(resids[[".resid"]], as.numeric(residuals(mbl[[attr(mbl,"model")[[1]]]][[1]])[[".resid"]]))
  
  resids <- residuals(mbl_multi)
  expect_true(is_tsibble(resids))
  expect_equal(key_vars(resids), c("key", ".model"))
  expect_true(all(colnames(resids) %in% c("key", ".model", "index", ".resid")))
  expect_equal(unique(resids[["key"]]), mbl_multi[["key"]])
  expect_equal(resids[["index"]], lung_deaths_long_tr[["index"]])
  expect_equal(resids[[".resid"]], 
               as.numeric(c(
                 residuals(mbl_multi[[attr(mbl,"model")[[1]]]][[1]])[[".resid"]],
                 residuals(mbl_multi[[attr(mbl,"model")[[1]]]][[2]])[[".resid"]]
               ))
  )
})

test_that("mable dplyr verbs", {
  skip_if_not_installed("fable")
  library(dplyr)
  expect_output(mbl_complex %>% select(key, ets) %>% print, "mable: 2 x 2") %>% 
    colnames %>% 
    expect_identical(c("key", "ets"))
  
  expect_output(mbl_complex %>% select(key, ets) %>% print, "mable: 2 x 2") %>% 
    colnames %>% 
    expect_identical(c("key", "ets"))
  
  # Test for negative tidyselect with keyed data (#120)
  mbl_complex %>% 
    select(-lm) %>%
    colnames() %>% 
    expect_identical(c("key", "ets"))
  
  # expect_error(select(mbl_complex, -key),
  #              "not a valid mable")
  
  expect_output(mbl_complex %>% filter(key == "mdeaths") %>% print, "mable") %>% 
    .[["key"]] %>% 
    expect_identical("mdeaths")
})