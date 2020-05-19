context("test-fable")

test_that("fable dplyr verbs", {
  skip_if_not_installed("fable")
  
  fbl_complex %>% filter(key == "mdeaths") %>% 
    expect_s3_class("fbl_ts") %>% 
    NROW %>% 
    expect_equal(24)
  
  # tsibble now automatically selects keys
  # expect_error(
  #   fbl_complex %>% select(index, .model, value, .distribution),
  #   "not a valid tsibble"
  # )
  
  fbl_complex %>%
    filter(key == "mdeaths") %>%
    select(index, .model, value, .mean) %>% 
    n_keys() %>% 
    expect_equal(2)
  
  expect_equal(
    colnames(hilo(fbl_complex, level = c(50, 80, 95))),
    c("key", ".model", "index", "value", ".mean", "50%", "80%", "95%")
  )
  
  expect_equivalent(
    as.list(fbl_multi),
    as.list(bind_rows(fbl_multi[1:12,], fbl_multi[13:24,]))
  )
})
