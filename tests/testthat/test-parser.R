context("test-parser.R")

test_that("Model parsing variety", {
  # Parse with no rhs and no specials
  parse1 <- model(us_deaths, no_specials(value))
  expect_equal(parse1[[1]][[1]]$fit, list())
  
  # Parse with no specials
  expect_warning(model(us_deaths, no_specials(value ~ rhs)), 
               "Exogenous regressors are not supported")
  
  # Parse xreg
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
  log_trans <- new_transformation(
    function(.x) log(.x),
    function(.x) exp(.x)
  )
  expect_equal(parse_log1[[1]][[1]]$transformation[[1]], log_trans)
  expect_equal(parse_log1[[1]][[1]]$response[[1]], as.name("value"))
  
  
  # Parse lhs transformation with rhs
  parse_log2 <- model(us_deaths, specials(log(value) ~ 1))
  expect_equal(parse_log1[[1]][[1]]$transformation[[1]], parse_log2[[1]][[1]]$transformation[[1]])
  expect_equal(parse_log1[[1]][[1]]$response[[1]], parse_log2[[1]][[1]]$response[[1]])
  
  # Parse lhs transformation with specials
  parse_log3 <- model(us_deaths, specials(log(value) ~ value + log(value) + rnorm(0,1) + log5(value)))
  expect_equal(parse_log1[[1]][[1]]$transformation[[1]], parse_log3[[1]][[1]]$transformation[[1]])
  expect_equal(parse_log1[[1]][[1]]$response[[1]], parse_log3[[1]][[1]]$response[[1]])
})


test_that("Model parsing scope", {
  # Test scoping without provided formula
  mdl <- eval({
    model(us_deaths, no_specials())
  }, envir = new_environment(list(no_specials = no_specials)))
  expect_equal(mdl[[1]][[1]]$response[[1]], sym("value"))
  
  mdl <- eval({
    model(us_deaths, no_specials(value))
  }, envir = new_environment(list(no_specials = no_specials)))
  expect_equal(mdl[[1]][[1]]$response[[1]], sym("value"))
  
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
  
  expect_equal(mdl[[1]][[1]]$response[[1]], sym("something"))
  
  # Transformation from scalar
  mdl <- eval({
    scale <- pi
    model(us_deaths, no_specials(value/scale))
  }, envir = new_environment(list(no_specials = no_specials)))
  
  expect_equal(mdl[[1]][[1]]$response[[1]], sym("value"))
  
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
  dt <- as_tsibble(list(index = Sys.Date() - 1:10, GDP = rnorm(10), CPI = rnorm(10)))
  
  # Untransformed response
  mdl <- model(dt, no_specials(GDP))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP))
  mdl <- model(dt, no_specials(resp(GDP)))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP))
  
  # Scalar transformed response
  mdl <- model(dt, no_specials(GDP/pi))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP))
  mdl <- model(dt, no_specials(resp(GDP)/pi))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP))
  
  # Transformation with a tie
  mdl <- model(dt, no_specials(GDP/CPI))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP/CPI))
  mdl <- model(dt, no_specials(resp(GDP)/CPI))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(GDP))
  expect_equal(mdl[[1]][[1]]$transformation[[1]], 
               new_transformation(
                 function(.x) .x/CPI,
                 function(.x) CPI * .x
               ))
  mdl <- model(dt, no_specials(GDP/resp(CPI)))
  expect_equal(mdl[[1]][[1]]$response[[1]], expr(CPI))
  expect_equal(mdl[[1]][[1]]$transformation[[1]], 
               new_transformation(
                 function(.x) GDP/.x,
                 function(.x) GDP/.x
               ))
})
