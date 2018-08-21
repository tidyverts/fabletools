context("test-summary.R")

test_that("Summarising mables / fables", {
  fc_sum <- fbl %>% summary
  
  expect_equal(dim(fc_sum), c(24, 4))
  expect_s3_class(fc_sum$`80%`, "hilo")
})
