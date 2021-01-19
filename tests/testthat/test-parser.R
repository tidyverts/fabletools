context("test-parser.R")

test_that("Model parsing variety", {
  # Parse with no rhs and no specials
  parse1 <- model(us_deaths, no_specials(value))
  expect_equal(parse1[[1]][[1]]$fit, list())
  
  # Parse with no specials
  expect_warning(model(us_deaths, no_specials(value ~ rhs)), 
                 "Exogenous regressors are not supported")
  
  # Parse xreg
  parse_xreg <- model(us_deaths, specials(value ~ xreg(value, 3) + log(value)))
  expect_identical(parse_xreg[[1]][[1]]$fit$xreg[[1]], "xreg(value, 3)")
  
  parse_xreg <- model(us_deaths, specials(value ~ value + log(value)))
  expect_identical(parse_xreg[[1]][[1]]$fit$xreg[[1]], "xreg(value, log(value))")
  
  # Parse special
  parse_log5 <- model(us_deaths, specials(value ~ log5(value)))
  expect_identical(parse_log5[[1]][[1]]$fit$log5[[1]], logb(us_deaths$value, 5))
  
  # Parse specials using .vals
  parse_rnorm <- model(us_deaths, specials(value ~ rnorm(0,1)))
  expect_length(parse_rnorm[[1]][[1]]$fit$rnorm[[1]], NROW(us_deaths))
  
  # Parse multiple specials
  parse_multi <- model(us_deaths, specials(value ~ value + log(value) + rnorm(0,1) + log5(value)))
  expect_length(parse_multi[[1]][[1]]$fit, 3)
  expect_identical(parse_xreg[[1]][[1]]$fit$xreg[[1]], parse_multi[[1]][[1]]$fit$xreg[[1]])
  expect_identical(parse_log5[[1]][[1]]$fit$log5[[1]], parse_multi[[1]][[1]]$fit$log5[[1]])
  expect_identical(length(parse_rnorm[[1]][[1]]$fit$rnorm[[1]]), length(parse_multi[[1]][[1]]$fit$rnorm[[1]]))
  
  # Special causing error
  expect_warning(model(us_deaths, specials(value ~ oops())), "Not allowed")
  
  # Parse lhs transformation with no rhs
  parse_log1 <- model(us_deaths, specials(log(value)))
  mdl1_trans <- parse_log1[[1]][[1]]$transformation[[1]]
  log_trans <- new_transformation(
    function(value) log(value),
    function(value) exp(value)
  )
  expect_identical(capture.output(mdl1_trans), capture.output(log_trans))
  expect_equal(response_vars(parse_log1), "value")
  
  # Parse lhs transformation with rhs
  parse_log2 <- model(us_deaths, specials(log(value) ~ 1))
  mdl2_trans <- parse_log2[[1]][[1]]$transformation[[1]]
  expect_identical(capture.output(mdl1_trans), capture.output(mdl2_trans))
  expect_identical(response_vars(parse_log1), response_vars(parse_log2))
  
  # Parse lhs transformation with specials
  parse_log3 <- model(us_deaths, specials(log(value) ~ value + log(value) + rnorm(0,1) + log5(value)))
  mdl3_trans <- parse_log3[[1]][[1]]$transformation[[1]]
  expect_identical(capture.output(mdl1_trans), capture.output(mdl3_trans))
  expect_identical(response_vars(parse_log1), response_vars(parse_log3))
})


test_that("Model parsing scope", {
  # Test scoping without provided formula
  mdl <- eval({
    model(us_deaths, no_specials())
  }, envir = new_environment(list(no_specials = no_specials)))
  expect_equal(response_vars(mdl), "value")
  
  mdl <- eval({
    model(us_deaths, no_specials(value))
  }, envir = new_environment(list(no_specials = no_specials)))
  expect_equal(response_vars(mdl), "value")
  
  expect_error(
    eval({
      model(us_deaths, no_specials(nothing))
    }, envir = new_environment(list(no_specials = no_specials))),
    "nothing"
  )
  
  # Response variable from env
  mdl <- eval({
    something <- 1:72
    model(us_deaths, no_specials(something))
  }, envir = new_environment(list(no_specials = no_specials)))
  
  expect_equal(response_vars(mdl), "something")
  
  # Transformation from scalar
  mdl <- eval({
    scale <- pi
    model(us_deaths, no_specials(value/scale))
  }, envir = new_environment(list(no_specials = no_specials)))
  
  expect_equal(response_vars(mdl), "value")
  
  # Transformation from scalar in function env
  mdl <- eval({
    {function() {
      scale <- pi
      model(us_deaths, no_specials(value/scale))
    }} ()
  }, envir = new_environment(list(no_specials = no_specials)))
  
  expect_equal(response_vars(mdl), "value")
  
  # Specials missing values
  expect_warning(
    eval({
      model(us_deaths, specials(value ~ log5(mytrend)))
    }, envir = new_environment(list(specials = specials))),
    "mytrend"
  )
  
  # Specials with data from scope
  mdl <- eval({
    mytrend <- 1:72
    model(us_deaths, specials(value ~ log5(mytrend)))
  }, envir = new_environment(list(specials = specials)))
  
  expect_equal(mdl[[1]][[1]]$fit[[1]][[1]], log(1:72, 5))
})


test_that("Model response identification", {
  dt <- tsibble(
    idx = Sys.Date() - 1:10, GDP = rnorm(10), CPI = rnorm(10),
    index = idx
  )
  
  # Untransformed response
  mdl <- model(dt, no_specials(GDP))
  expect_equal(response_vars(mdl), "GDP")
  mdl <- model(dt, no_specials(resp(GDP)))
  expect_equal(response_vars(mdl), "GDP")
  
  # Scalar transformed response
  mdl <- model(dt, no_specials(GDP/pi))
  expect_equal(response_vars(mdl), "GDP")
  mdl <- model(dt, no_specials(resp(GDP)/pi))
  expect_equal(response_vars(mdl), "GDP")
  
  # Transformation with a tie
  mdl <- model(dt, no_specials(GDP/CPI))
  expect_equal(response_vars(mdl), "GDP/CPI")
  mdl <- model(dt, no_specials(resp(GDP)/CPI))
  mdl_trans <- mdl[[1]][[1]]$transformation[[1]]
  cpi_trans <- new_transformation(
    function(GDP) GDP/CPI,
    function(GDP) CPI * GDP
  )
  expect_identical(response_vars(mdl), "GDP")
  expect_identical(capture.output(mdl_trans), capture.output(cpi_trans))
  mdl <- model(dt, no_specials(GDP/resp(CPI)))
  mdl_trans <- mdl[[1]][[1]]$transformation[[1]]
  gdp_trans <- new_transformation(
    function(CPI) GDP/CPI,
    function(CPI) GDP/CPI
  )
  expect_equal(response_vars(mdl), "CPI")
  expect_identical(capture.output(mdl_trans), capture.output(gdp_trans))
})

