context("test-combination")

test_that("Combination modelling", {
  skip_if_not_installed("fable")
  
  mbl_cmbn <- mbl %>% 
    transmute(combination = (ets + ets)/2)
  
  expect_equal(
    augment(mbl_cmbn, type = "response")[,-1],
    augment(mbl, type = "response")[,-1]
  )
  
  expect_equivalent(
    unclass(ggplot2::fortify(mbl_cmbn %>% forecast(h = 12))[,-1]),
    unclass(ggplot2::fortify(fbl)[,-1])
  )
  
  mbl_cmbn <- us_deaths_tr %>% 
    model(
      a = fable::SNAIVE(value),
      b = combination_model(fable::SNAIVE(value), fable::SNAIVE(value))
    )
  
  fbl_cmbn <- forecast(mbl_cmbn)
  
  expect_equivalent(
    unclass(ggplot2::fortify(fbl_cmbn)[1:48, -1]),
    unclass(ggplot2::fortify(fbl_cmbn)[49:96, -1])
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
    fbl_cmbn$value,
    (fbl_snaive$value + fbl_rw$value)/2
  )
  expect_failure(
    expect_equal(
      fbl_wt_cmbn$value,
      fbl_cmbn$value
    )
  )
})
