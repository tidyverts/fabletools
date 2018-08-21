context("test-parser.R")

test_that("Model parser", {
  specials <- new_specials_env(
    rnorm = function(m,s){
      rnorm(NROW(.data), m, s)
    },
    log5 = function(x){
      logb(x, base = 5)
    },
    oops = function(){
      stop("Not allowed")
    },
    xreg = function(...){
      deparse(match.call())
    },
    .vals = list(.data = USAccDeaths)
  )
  # Parse with no rhs and no specials
  parse1 <- parse_model(USAccDeaths, as.name("value"))
  
  # Parse with no specials
  parse2 <- parse_model(USAccDeaths, value ~ rhs)
  
  expect_equal(parse1[-1], parse2[-1])
  
  # Parse xreg
  parse_xreg <- parse_model(USAccDeaths, value ~ value + log(value), specials = specials)
  expect_identical(parse_xreg$specials$xreg[[1]], "xreg(value, log(value))")
  
  # Parse special
  parse_log5 <- parse_model(USAccDeaths, value ~ log5(value), specials = specials)
  expect_identical(parse_log5$specials$log5[[1]], logb(USAccDeaths$value, 5))
  
  # Parse specials using .vals
  parse_rnorm <- parse_model(USAccDeaths, value ~ rnorm(0,1), specials = specials)
  expect_length(parse_rnorm$specials$rnorm[[1]], NROW(USAccDeaths))
  
  # Parse multiple specials
  parse_multi <- parse_model(USAccDeaths, value ~ value + log(value) + rnorm(0,1) + log5(value), specials = specials)
  expect_length(parse_multi$specials, 3)
  expect_identical(parse_xreg$specials$xreg[[1]], parse_multi$specials$xreg[[1]])
  expect_identical(parse_log5$specials$log5[[1]], parse_multi$specials$log5[[1]])
  expect_identical(length(parse_rnorm$specials$rnorm[[1]]), length(parse_multi$specials$rnorm[[1]]))
  
  # Special causing error
  expect_error(parse_model(USAccDeaths, value ~ oops(), specials = specials), "Not allowed")
  
  # Parse lhs transformation with no rhs
  parse_log1 <- parse_model(USAccDeaths, rlang::expr(log(value)))
  value <- rpois(10, 1)
  expect_equal(parse_log1$transformation, as_transformation(log(value)))
  expect_equal(parse_log1$response, as.name("value"))
  
  
  # Parse lhs transformation with rhs
  parse_log2 <- parse_model(USAccDeaths, log(value) ~ 1)
  expect_equal(parse_log1$transformation, parse_log2$transformation)
  expect_equal(parse_log1$response, parse_log2$response)
  
  # Parse lhs transformation with specials
  parse_log3 <- parse_model(USAccDeaths, log(value) ~ value + log(value) + rnorm(0,1) + log5(value), specials = specials)
  expect_equal(parse_log1$transformation, parse_log3$transformation)
  expect_equal(parse_log1$response, parse_log3$response)
})
