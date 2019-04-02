context("test-validate_model.R")

test_that("validate_model", {
  # Test expression capturing
  expect_identical(validate_formula(no_specials(user_model)), as.name("user_model"))
  
  # Test formula evaluating
  user_model <- y~x
  expect_identical(validate_formula(no_specials(user_model)), user_model)
  
  # Test bare formula
  expect_identical(validate_formula(no_specials(y~x)), y~x)
  
  tsbl1 <- tsibble::tsibble(
    date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
    value = rnorm(10), index = date
  )
  
  # Test automatic response selection
  expect_message(res <- validate_formula(no_specials(), data = tsbl1),
                 "Model not specified, defaulting to automatic modelling of the `value` variable.")
  expect_identical(res, as.name("value"))
  
  # Test LHS automatic response selection
  expect_message(res <- validate_formula(no_specials(~x), data = tsbl1), "Model not specified, defaulting to automatic modelling of the `value` variable.")
  expect_identical(res, value ~ x)
  
  # Test failed response selection
  tsbl1[["value2"]] <- rnorm(10) 
  expect_error(validate_formula(no_specials(), data = tsbl1), "Could not automatically determine the response variable")
  
  # Test failed LHS response variable selection
  expect_error(validate_formula(no_specials(~ x), data = tsbl1), "Could not automatically determine the response variable")
  
  # Test not supported automatic variable selection
  expect_error(validate_formula(no_specials(), data = NULL), "This model function does not support automatic selection")
})