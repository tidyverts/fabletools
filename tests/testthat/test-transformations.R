context("test-transformations")

simple_data <- tsibble(idx = 1:10, y = abs(rnorm(10)), x = 1:10, index = idx)
test_transformation <- function(..., dt = simple_data){
  mdl <- estimate(dt, no_specials(...))
  trans <- mdl$transformation[[1]]
  resp <- mdl$response[[1]]
  expect_equal(
    dt[[expr_text(resp)]],
    fabletools:::invert_transformation(trans)(trans(dt[[expr_text(resp)]]))
  )
}

test_that("single transformations", {
  test_transformation(y)
  test_transformation(y + 10)
  test_transformation(10 + y)
  test_transformation(+y)
  test_transformation(y - 10)
  test_transformation(10 - y)
  test_transformation(-y)
  test_transformation(3*y)
  test_transformation(y*3)
  test_transformation(3/y)
  test_transformation(y/3)
  test_transformation(log(y))
  test_transformation(logb(y, 10))
  test_transformation(log10(y))
  test_transformation(log2(y))
  test_transformation(log1p(y))
  test_transformation(expm1(y))
  test_transformation(exp(y))
  test_transformation(box_cox(y, 0.4))
  test_transformation(inv_box_cox(y, 0.4))
  test_transformation(sqrt(y))
  test_transformation(y^2)
  test_transformation(2^y)
  test_transformation((y))
})


test_that("transformation chains", {
  test_transformation(y + 10 - 10)
  test_transformation(10 + y * 10)
  test_transformation(+y - y)
  test_transformation(y^2 + 3)
  test_transformation(log(sqrt(y)))
  test_transformation(log(y + 1))
  test_transformation(box_cox(y^2,0.3))
  test_transformation(box_cox(y,0.3) + 1)
  
  # Something too complex
  expect_error(
    test_transformation(box_cox(y,0.3)^2),
    "Could not identify a valid back-transformation"
  )
  
  # Something rediculous
  test_transformation(log(sqrt(sqrt(sqrt(sqrt(sqrt(y)))+3))))
})