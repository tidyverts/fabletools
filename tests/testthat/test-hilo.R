context("test-hilo.R")

test_that("Extracting intervals from a distribution", {
  skip_if_not_installed("fable")
  
  fc_sum <- fbl %>% 
    mutate(`80%` = hilo(value, 80))
  
  expect_s3_class(fc_sum$`80%`, "hilo")
})
