context("test-decomposition-model")

test_that("Decomposition modelling", {
  skip_if_not_installed("fable")
  skip_if_not_installed("feasts")
  
  mdl_dcmp <- us_deaths %>%
    model(decomposition_model(feasts::STL(value), fable::NAIVE(season_adjust)))
  
  expect_output(
    report(mdl_dcmp),
    "Series: season_adjust \\nModel: NAIVE"
  )
  expect_output(
    report(mdl_dcmp),
    "Series: season_year \\nModel: SNAIVE"
  )
  
  fbl_dcmp <- forecast(mdl_dcmp)
  
  expect_equal(
    mean(fbl_dcmp$value),
    rep(dcmp$season_year[61:72], 2) + dcmp$season_adjust[72]
  )
})
