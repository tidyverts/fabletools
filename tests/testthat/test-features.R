context("test-features")

www_usage <- as_tsibble(WWWusage)
lung_deaths_long <- as_tsibble(cbind(mdeaths, fdeaths))
lung_deaths_wide <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)

first <- function(x) c(first = x[1])
last <- function(x) c(last = x[length(x)])

test_that("features()", {
  expect_error(
    features(lung_deaths_wide, vars(mdeaths, fdeaths), first),
    "only supports a single variable"
  )
  
  expect_message(
    features(lung_deaths_wide, features = list(first, last)),
    "Feature variable not specified, automatically selected \\`.var = mdeaths\\`"
  ) %>%
    colnames() %>%
    expect_equal(c("first", "last"))
  
  features(lung_deaths_wide, mdeaths, features = list(a = mean, b = min, max)) %>% 
    colnames() %>% 
    expect_equal(c("a", "b", "V1"))
})

test_that("Scoped variants of features()", {
  ft_at <- features_at(lung_deaths_wide, vars(mdeaths:fdeaths), list(first, last))
  expect_equal(
    substr(colnames(ft_at), 1, 7),
    c(rep("mdeaths", 2), rep("fdeaths", 2))
  )
  ft_if <- features_if(lung_deaths_wide, is.numeric, list(first, last))
  expect_identical(
    ft_at, ft_if
  )
  ft_all <- features_all(lung_deaths_wide, list(first, last))
  expect_identical(
    ft_if, ft_all
  )
})