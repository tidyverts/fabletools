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