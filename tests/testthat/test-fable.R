context("test-fable")

test_that("fable dplyr verbs", {
  fbl_complex %>% filter(key == "mdeaths") %>% 
    expect_s3_class("fbl_ts") %>% 
    NROW %>% 
    expect_equal(48)
  
  expect_error(
    fbl_complex %>% select(index, .model, value, .distribution),
    "not a valid tsibble"
  )
  
  fbl_complex %>%
    filter(key == "mdeaths") %>%
    select(index, .model, value, .distribution) %>% 
    n_keys() %>% 
    expect_equal(2)
})
