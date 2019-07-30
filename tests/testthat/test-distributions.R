context("test-distributions")

test_that("Normal distribution", {
  d <- dist_normal(3, 5)
  expect_output(
    print(d),
    "N\\(3, 25\\)"
  )
  expect_output(
    print(hilo(d)),
    "[-6.79982, 12.79982]",
  )
  d <- d + d
  expect_output(
    print(d),
    "N\\(6, 50\\)"
  )
  d <- 3*d
  expect_output(
    print(d),
    "N\\(18, 450\\)"
  )
  expect_output(
    print(d-d),
    "N\\(0, 900\\)"
  )
  expect_warning(
    d*d,
    "Multiplying forecast distributions is not supported"
  ) %>% 
    fabletools:::is_dist_unknown() %>% 
    expect_true()
  
  expect_length(rep(d, 10), 10)
  
  expect_length(sample(rep(d, 10), 4), 4)
  
  # expect_equal(
  #   unique(rep(d, 10)),
  #   d
  # )
  
  td <- fabletools:::update_fcdist(d, transformation = list(function(x) exp(x)))
  
  expect_equal(
    hilo(d)[[1]] %>% mutate(.lower = exp(.lower), .upper = exp(.upper)),
    hilo(td)[[1]]
  )
  
  expect_warning(
    d + td,
    "Combinations of non-normal forecast distributions is not supported."
  ) %>% 
    expect_equal(
      dist_unknown(1)
    )
  
  expect_equal(
    hilo(c(d, td)),
    c(hilo(d), hilo(td))
  )
})

test_that("Sample distribution", {
  d <- dist_sim(list(0:100, -100:100, 400:450))
  expect_length(d, 3)
  expect_length(rep(d, 3), 9)
  expect_output(
    print(hilo(d[1])),
    "[2.5, 9.75]"
  )
  expect_output(
    print(hilo(d[2])),
    "[-95, 95]"
  )
})

test_that("Unknown distribution", {
  d <- dist_unknown(1)
  expect_output(print(d), "?")
  expect_output(
    print(hilo(d)),
    "[NA, NA]"
  )
})
