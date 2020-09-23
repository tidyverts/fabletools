context("test-combination")

test_that("Combination modelling", {
  skip_if_not_installed("fable")
  
  mbl_cmbn <- mbl %>% 
    transmute(combination = (ets + ets)/2)
  
  expect_equal(
    select(augment(mbl_cmbn), -.model, -.innov),
    select(augment(mbl), -.model, -.innov)
  )
  
  expect_equivalent(
    forecast(mbl_cmbn, h = 12)[,-1],
    fbl[,-1]
  )
  
  mbl_cmbn <- us_deaths_tr %>% 
    model(
      a = fable::SNAIVE(value),
      b = combination_model(fable::SNAIVE(value), fable::SNAIVE(value))
    )
  
  fbl_cmbn <- forecast(mbl_cmbn)
  
  expect_equivalent(
    fbl_cmbn[1:24, -1],
    fbl_cmbn[25:48, -1]
  )
  
  mbl_cmbn <- us_deaths_tr %>% 
    model(
      snaive = fable::SNAIVE(value),
      rw = fable::RW(value ~ drift()),
      cmbn = (fable::SNAIVE(value) + fable::RW(value ~ drift()))/2
    ) %>% 
    mutate(combination_ensemble(snaive, rw, weights = "inv_var"))
  fbl_snaive <- forecast(select(mbl_cmbn, 1))
  fbl_rw <- forecast(select(mbl_cmbn, 2))
  fbl_cmbn <- forecast(select(mbl_cmbn, 3))
  fbl_wt_cmbn <- forecast(select(mbl_cmbn, 4))
  expect_equal(
    mean(fbl_cmbn$value),
    mean((fbl_snaive$value + fbl_rw$value)/2)
  )
  expect_failure(
    expect_equal(
      mean(fbl_wt_cmbn$value),
      mean(fbl_cmbn$value)
    )
  )
})
