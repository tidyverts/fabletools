context("test-broom")

test_that("augment", {
  skip_if_not_installed("fable")
  
  aug <- augment(mbl)
  expect_equal(aug$index, us_deaths_tr$index)
  expect_equal(aug$.fitted, fitted(mbl)$.fitted)
  expect_equal(aug$.resid, residuals(mbl, type ="response")$.resid)
  expect_equal(aug$.innov, residuals(mbl)$.resid)
  
  aug <- augment(mbl_multi)
  expect_equal(aug$index, lung_deaths_long_tr$index)
  expect_equal(aug$.fitted, fitted(mbl_multi)$.fitted)
  expect_equal(aug$.resid, residuals(mbl_multi, type = "response")$.resid)
  expect_equal(aug$.innov, residuals(mbl_multi)$.resid)
  
  aug <- augment(mbl_complex)
  expect_equal(aug$index, rep(lung_deaths_long_tr$index, 2))
  expect_equal(aug$.fitted, fitted(mbl_complex)$.fitted)
  expect_equal(aug$.resid, residuals(mbl_complex, type = "response")$.resid)
  expect_equal(aug$.innov, residuals(mbl_complex)$.resid)

  aug <- augment(mbl_mv)
  expect_equal(aug$index, rep(lung_deaths_wide_tr$index, 2))
  expect_equal(aug$.fitted, c(fitted(mbl_mv)$mdeaths, fitted(mbl_mv)$fdeaths))
  expect_equal(aug$.resid, c(residuals(mbl_mv)$mdeaths, residuals(mbl_mv)$fdeaths))
})

test_that("glance", {
  skip_if_not_installed("fable")
  
  gl <- glance(mbl)
  expect_equal(NROW(gl), 1)
  gl_multi <- glance(mbl_multi)
  expect_equal(NROW(gl_multi), 2)
  expect_equal(gl_multi$key, c("fdeaths", "mdeaths"))
  gl_complex <- glance(mbl_complex)
  expect_equal(NROW(gl_complex), 4)
  expect_equal(gl_complex$key, rep(c("fdeaths", "mdeaths"), each = 2))
  expect_equal(gl_multi[-2], gl_complex[c(1,3), names(gl_multi)][-2])
  
  gl_mv <- glance(mbl_mv)
  expect_equal(NROW(gl_mv), 1)
})

test_that("tidy", {
  skip_if_not_installed("fable")
  
  td <- tidy(mbl)
  expect_equal(unique(td$.model), "ets")
  td_multi <- tidy(mbl_multi)
  expect_equal(unique(td_multi$.model), "ets")
  expect_equal(unique(td_multi$key), c("fdeaths", "mdeaths"))
  td_complex <- tidy(mbl_complex)
  expect_equal(unique(td_complex$.model), c("ets", "lm"))
  expect_equal(unique(td_complex$key), c("fdeaths", "mdeaths"))
  
  td_mv <- tidy(mbl_mv)
  expect_equal(unique(td_mv$.model), "var")
  expect_equal(unique(td_mv$.response), c("mdeaths", "fdeaths"))
})
