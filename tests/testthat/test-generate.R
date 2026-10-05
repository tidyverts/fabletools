context("test-generate")

test_that("generate", {
  skip_if_not_installed("fable")
  
  gen <- mbl %>% generate()
  expect_equal(NROW(gen), 24)
  expect_equal(gen$index, yearmonth("1978 Jan") + 0:23)
  
  gen_multi <- mbl_multi %>% generate()
  expect_equal(NROW(gen_multi), 48)
  expect_equal(gen_multi$index, yearmonth("1979 Jan") + rep(0:23, 2))
  expect_equal(unique(gen_multi$key), c("fdeaths", "mdeaths"))
  
  gen_complex <- mbl_complex %>% generate(times = 3)
  expect_equal(NROW(gen_complex), 24*2*2*3)
  expect_equal(gen_complex$index, yearmonth("1979 Jan") + rep(0:23, 2*2*3))
  expect_equal(unique(gen_complex$key), c("fdeaths", "mdeaths"))
  expect_equal(unique(gen_complex$.model), c("ets", "lm"))
})

test_that("generate seed setting", {
  skip_if_not_installed("fable")

  seed <- rnorm(1)
  expect_warning(
    gen1 <- mbl %>% generate(seed = seed),
    "deprecated"
  )
  # lifecycle only warns once per session for a given deprecation.
  gen2 <- suppressWarnings(mbl %>% generate(seed = seed))
  expect_equal(gen1, gen2)

  expect_failure(
    expect_equal(
      mbl %>% generate(),
      mbl %>% generate()
    )
  )
})

test_that("generate(seed = ) restores the global RNG state on exit", {
  skip_if_not_installed("fable")

  before <- .GlobalEnv$.Random.seed
  suppressWarnings(invisible(mbl %>% generate(seed = 123)))
  expect_identical(before, .GlobalEnv$.Random.seed)
})

test_that("generate() errors informatively when `.model` or `.rep` is already used (#275)", {
  skip_if_not_installed("fable")

  dt <- tsibble::tsibble(
    .model = rep(c("a", "b"), each = 10), t = rep(1:10, 2), y = 1:20,
    key = .model, index = t
  )
  expect_error(
    generate(model(dt, naive = fable::NAIVE(y)), h = 2),
    "key variable named `.model`"
  )

  # Modelling simulated paths gives a `.rep` key
  dt1 <- tsibble::tsibble(t = 1:10, y = 1:10, index = t)
  sim <- generate(model(dt1, naive = fable::NAIVE(y)), h = 5, times = 2)
  sim <- dplyr::select(sim, -.model)
  mbl_rep <- model(sim, naive = fable::NAIVE(.sim))
  expect_error(generate(mbl_rep, h = 2), "key variable named `.rep`")
  # forecast() does not add `.rep`, so is unaffected
  expect_s3_class(forecast(mbl_rep, h = 2), "fbl_ts")

  # A `.rep` column in new_data is still allowed to specify the replications
  nd <- dplyr::mutate(tsibble::new_data(dt1, 2), .rep = "1")
  gen <- generate(model(dt1, naive = fable::NAIVE(y)), new_data = nd)
  expect_equal(NROW(gen), 2)
})
test_that("generate() ignores `h` when `new_data` is provided, as forecast() does", {
  skip_if_not_installed("fable")

  nd <- tsibble::new_data(us_deaths_tr, 3)
  expect_warning(
    gen <- generate(mbl, new_data = nd, h = 12),
    "`h` will be ignored"
  )
  expect_equal(NROW(gen), 3)
})

test_that("generate() works with model columns of different classes (#408)", {
  skip_if_not_installed("fable")

  fit <- lung_deaths_long %>%
    aggregate_key(key, value = sum(value)) %>%
    model(snaive = fable::SNAIVE(value)) %>%
    mutate(bu = reconcile_bu(snaive), td = reconcile_td(snaive))
  gen <- generate(fit, h = 2, times = 3)
  expect_equal(unique(gen$.model), c("snaive", "bu", "td"))
  expect_equal(NROW(gen), 3 * 3 * 2 * 3)
})
