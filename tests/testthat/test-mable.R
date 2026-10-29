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

  # [<- registers model columns
  m_sub <- m1
  m_sub["b"] <- list(m2[["b"]])
  expect_identical(mable_vars(m_sub), c("a", "b"))
  expect_identical(m_sub, m_dol)

  # Removed model columns are no longer registered
  expect_identical(mable_vars(mutate(m_dbl, a = NULL)), "b")
  expect_identical(unique(forecast(mutate(m_dbl, a = NULL), h = 1)[[".model"]]), "b")
  m_sub["a"] <- NULL
  expect_identical(mable_vars(m_sub), "b")
  m_dol$a <- NULL
  expect_identical(mable_vars(m_dol), "b")
  expect_identical(mable_vars(mutate(m_dbl, a = 1)), "b")

  # Models are in the order of their columns
  m_rel <- relocate(m_dbl, b, .before = a)
  expect_identical(mable_vars(m_rel), c("b", "a"))
  expect_identical(unique(forecast(m_rel, h = 1)[[".model"]]), c("b", "a"))

  # Model columns are added with mutate(), not cbind()
  expect_error(cbind(m1, b = m2$b), "can't be added to a mable")

  # Column names must be unique
  expect_error(names(m_dbl) <- c("key", "a", "a", "c")[seq_along(m_dbl)], "must be unique")
})
test_that("Modelling data without any series gives an empty mable (#313)", {
  skip_if_not_installed("fable")
  empty <- tsibble::tsibble(i = 1:12, k = 1, y = 1, z = 1, index = "i", key = "k")[0,]

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
    model(empty, fable::MEAN(y), fable::MEAN(log(z))),
    "same response"
  )

  # Mables without models built by as_mable() have no structure for results
  no_mdl <- as_mable(tibble::tibble(m = new_mdl_lst()), model = "m", response = "y")
  expect_error(forecast(no_mdl, h = 1), "structure of its models is unknown")

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

  # Missing models give missing forecasts and summaries
  fc <- forecast(res, h = 2)
  expect_identical(NROW(fc), 8L)
  is_missing <- fc$key == res$key[1] & fc$.model == "b"
  expect_identical(sum(is_missing), 2L)
  expect_true(all(is.na(fc$.mean[is_missing])))
  expect_false(anyNA(fc$.mean[!is_missing]))
  fc <- forecast(res, new_data = tsibble::new_data(lung_deaths_long, 3))
  expect_identical(NROW(fc), 12L)
  expect_true(all(is.na(fc$.mean[fc$key == res$key[1] & fc$.model == "b"])))
  acc <- accuracy(res)
  expect_identical(NROW(acc), 4L)
  expect_true(all(is.nan(acc$RMSE[acc$key == res$key[1] & acc$.model == "b"])))
  expect_identical(NROW(glance(res)), 3L)
  expect_identical(NROW(tidy(res)), 0L)
  expect_identical(NROW(augment(res)), 3L * 72L)
})

test_that("Results of an empty mable have the structure of non-empty results (#313)", {
  skip_if_not_installed("fable")
  expect_same_structure <- function(empty, full) {
    expect_identical(NROW(empty), 0L)
    expect_identical(class(empty), class(full))
    # Column order can depend on the model, and future integer time indices
    # are doubles (from tsibble::new_data())
    expect_setequal(names(empty), names(full))
    expect_equal(vec_ptype(as_tibble(empty))[names(full)], vec_ptype(as_tibble(full)[0,]))
    if (tsibble::is_tsibble(full)) {
      expect_identical(key_vars(empty), key_vars(full))
      expect_identical(index_var(empty), index_var(full))
    }
    if (inherits(full, "fbl_ts")) {
      expect_identical(response_vars(empty), response_vars(full))
      expect_identical(distribution_var(empty), distribution_var(full))
    }
  }
  check_results <- function(empty, full, generate = TRUE) {
    expect_same_structure(forecast(empty, h = 2), forecast(full, h = 2))
    expect_same_structure(fitted(empty), fitted(full))
    expect_same_structure(residuals(empty), residuals(full))
    expect_same_structure(augment(empty), augment(full))
    expect_same_structure(accuracy(empty), accuracy(full))
    expect_same_structure(response(empty), response(full))
    if (generate) {
      expect_same_structure(generate(empty, h = 2, times = 2), generate(full, h = 2, times = 2))
    }
  }

  # Empty mables from model()
  dt <- tsibble::tsibble(i = 1:12, k = 1, y = 1:12 + 0, index = "i", key = "k")
  full <- model(dt, mean = fable::MEAN(y), naive = fable::NAIVE(log(y)))
  empty <- model(dt[0,], mean = fable::MEAN(y), naive = fable::NAIVE(log(y)))
  check_results(empty, full)

  # Empty mables from filtering
  check_results(filter(mbl_multi, key == "none"), mbl_multi)
  # Model specific columns (such as VAR's `.innov`) aren't simulated
  check_results(mbl_mv[0,], mbl_mv, generate = FALSE)
  expect_identical(NROW(generate(mbl_mv[0,], h = 2)), 0L)

  # new_data is used for structuring empty forecasts
  fc <- forecast(filter(mbl_multi, key == "none"),
                 new_data = mutate(new_data(lung_deaths_long_tr, 2), x = 1))
  expect_identical(NROW(fc), 0L)
  expect_true("x" %in% names(fc))

  # Components depend on the model, so can't be structured
  expect_error(components(empty), "without any models")

  # Interpolation gives the empty data
  expect_identical(NROW(interpolate(select(empty, k, mean), dt[0,])), 0L)

  # Specials which need observations are only used for the structure
  dt <- tsibble::tsibble(i = tsibble::yearquarter("2020 Q1") + 0:11, k = 1, y = 1:12 + 0, index = "i", key = "k")
  full <- model(dt, a = fable::TSLM(y ~ trend() + season()), b = fable::TSLM(y ~ fourier(K = 1)))
  empty <- model(dt[0,], a = fable::TSLM(y ~ trend() + season()), b = fable::TSLM(y ~ fourier(K = 1)))
  fc <- forecast(empty, h = 2)
  expect_identical(NROW(fc), 0L)
  expect_identical(names(fc), names(forecast(full, h = 2)))
  expect_same_structure(fitted(empty), fitted(full))
})

test_that("Combining mables with rbind() and vec_rbind() gives valid keys", {
  skip_if_not_installed("fable")
  mbl <- model(lung_deaths_long, snaive = fable::SNAIVE(value))
  res <- rbind(mbl[1,], mbl[2,])
  expect_s3_class(res, "mbl_df")
  expect_identical(n_keys(res), 2L)
  expect_identical(key_data(res), key_data(mbl))
  expect_error(rbind(mbl, mbl), "uniquely identify")

  res <- vctrs::vec_rbind(mbl[1,], mbl[2,])
  expect_identical(key_data(res), key_data(mbl))
  expect_identical(mable_vars(res), "snaive")

  # Slicing keeps the structure of the models
  empty <- vctrs::vec_slice(mbl, 0)
  expect_identical(n_keys(empty), 0L)
  expect_identical(NROW(forecast(empty, h = 1)), 0L)
})
