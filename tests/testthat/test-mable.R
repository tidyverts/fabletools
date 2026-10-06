context("test-mable.R")

test_that("Mable classes", {
  skip_if_not_installed("fable")
  expect_s3_class(mbl, "mbl_df")
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

test_that("Assigning model columns registers them as models (#402, #323)", {
  skip_if_not_installed("fable")
  m1 <- model(us_deaths_tr, a = fable::SNAIVE(value))
  m2 <- model(us_deaths_tr, b = fable::NAIVE(value))

  # [[<- with a character name
  m_dbl <- m1
  m_dbl[["b"]] <- m2[["b"]]
  expect_s3_class(m_dbl, "mbl_df")
  expect_identical(mable_vars(m_dbl), c("a", "b"))
  expect_identical(key_vars(m_dbl), key_vars(m1))
  expect_identical(unique(forecast(m_dbl, h = 1)[[".model"]]), c("a", "b"))

  # $<- still works
  m_dol <- m1
  m_dol$b <- m2$b
  expect_s3_class(m_dol, "mbl_df")
  expect_identical(mable_vars(m_dol), c("a", "b"))
  expect_identical(m_dol, m_dbl)

  # Replacing an existing model column keeps it registered
  m_rep <- m1
  m_rep[["a"]] <- m2[["b"]]
  expect_identical(mable_vars(m_rep), "a")

  # Non-model columns are not registered as models
  m_dbl[["c"]] <- 1
  expect_identical(mable_vars(m_dbl), c("a", "b"))
})
test_that("Modelling data without any series gives an empty mable (#313)", {
  skip_if_not_installed("fable")
  empty <- tsibble::tsibble(i = 1:12, k = 1, y = 1, index = "i", key = "k")[0,]

  mbl <- model(empty, mean = fable::MEAN(y), naive = fable::NAIVE(log(y)))
  expect_s3_class(mbl, "mbl_df")
  expect_identical(NROW(mbl), 0L)
  expect_identical(key_vars(mbl), "k")
  expect_identical(mable_vars(mbl), c("mean", "naive"))
  expect_identical(response_vars(mbl), "y")

  # The response is kept when the mable is modified
  expect_identical(response_vars(select(mbl, k, mean)), "y")
  expect_identical(response_vars(filter(mbl, k == 1)), "y")
  expect_identical(response_vars(mutate(mbl, snaive = mean)), "y")
  expect_identical(response_vars(mbl[c("k", "naive")]), "y")
  full <- model(
    tsibble::tsibble(i = 1:12, k = 1, y = 1, index = "i", key = "k"),
    mean = fable::MEAN(y), naive = fable::NAIVE(log(y))
  )
  expect_identical(NROW(bind_rows(mbl, full)), 1L)
  expect_identical(NROW(bind_rows(full, mbl)), 1L)

  # Models of an empty mable must still share a response
  expect_error(
    model(empty, fable::MEAN(y), fable::MEAN(log(k))),
    "same response"
  )

  # Results can't be structured without any models
  expect_error(forecast(mbl, h = 1), "without any models")

  expect_error(as_mable(tibble::tibble(m = new_mdl_lst()), model = "m"), "must be specified")
  expect_identical(
    response_vars(as_mable(tibble::tibble(m = new_mdl_lst()), model = "m", response = "y")),
    "y"
  )
})

test_that("Mables can be combined with missing models (#234)", {
  skip_if_not_installed("fable")
  mbl <- model(lung_deaths_long, a = fable::SNAIVE(value), b = fable::NAIVE(value))
  res <- bind_rows(mbl[1,], mbl[2, c("key", "a")])
  expect_identical(response_vars(res), "value")
  expect_null(res[["b"]][[2]])
  res <- bind_rows(mbl[2, c("key", "a")], mbl[1,])
  expect_identical(response_vars(res), "value")
  expect_null(res[["b"]][[1]])
})
