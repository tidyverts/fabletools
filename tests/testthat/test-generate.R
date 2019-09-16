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
  
  expect_error(
    mbl_mv %>% generate(),
    "Generating paths from multivariate models is not yet supported"
  )
})

test_that("generate seed setting", {
  skip_if_not_installed("fable")
  
  seed <- rnorm(1)
  expect_equal(
    mbl %>% generate(seed = seed),
    mbl %>% generate(seed = seed) 
  )
  
  expect_failure(
    expect_equal(
      mbl %>% generate(),
      mbl %>% generate()
    )
  )
})