context("test-hilo.R")

test_that("Extracting intervals from a distribution", {
  fc_sum <- fbl %>% 
    mutate("80%" := hilo(!!(fbl%@%"dist"), 80))
  
  expect_s3_class(fc_sum$`80%`, "hilo")
})
