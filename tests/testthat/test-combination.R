context("test-combination")

test_that("Joint combination: order-invariant variance (equal weights, 3 models)", {
  skip_if_not_installed("fable")
  
  # Three orderings of the same three component models
  mbl_cmbn <- us_deaths_tr |>
    model(
      ABC = combination_model(
        fable::SNAIVE(value), fable::RW(value ~ drift()), fable::ETS(value),
        cmbn_args = list(weights = "equal")
      ),
      BCA = combination_model(
        fable::RW(value ~ drift()), fable::ETS(value), fable::SNAIVE(value),
        cmbn_args = list(weights = "equal")
      ),
      CAB = combination_model(
        fable::ETS(value), fable::SNAIVE(value), fable::RW(value ~ drift()),
        cmbn_args = list(weights = "equal")
      )
    )
  fc <- forecast(mbl_cmbn, h = 6)
  
  vars <- distributional::variance(fc$value)
  abc  <- vars[fc$.model == "ABC"]
  bca  <- vars[fc$.model == "BCA"]
  cab  <- vars[fc$.model == "CAB"]
  
  expect_equal(abc, bca, tolerance = 1e-6,
    label = "equal-weight variance (ABC vs BCA)")
  expect_equal(abc, cab, tolerance = 1e-6,
    label = "equal-weight variance (ABC vs CAB)")
  
  # Means should also be identical regardless of order
  means <- mean(fc$value)
  expect_equal(means[fc$.model == "ABC"], means[fc$.model == "BCA"],
    tolerance = 1e-6)
})

test_that("Joint combination: order-invariant variance (inv_var weights, 3 models)", {
  skip_if_not_installed("fable")
  
  mbl_cmbn <- us_deaths_tr |>
    model(
      ABC = combination_model(
        fable::SNAIVE(value), fable::RW(value ~ drift()), fable::ETS(value),
        cmbn_args = list(weights = "inv_var")
      ),
      CAB = combination_model(
        fable::ETS(value), fable::SNAIVE(value), fable::RW(value ~ drift()),
        cmbn_args = list(weights = "inv_var")
      )
    )
  fc <- forecast(mbl_cmbn, h = 6)
  
  vars <- distributional::variance(fc$value)
  expect_equal(
    vars[fc$.model == "ABC"],
    vars[fc$.model == "CAB"],
    tolerance = 1e-6,
    label = "inv_var variance is order-invariant"
  )
})

test_that("Joint combination: sum/mean/weighted.mean methods work correctly", {
  skip_if_not_installed("fable")
  
  fit <- us_deaths_tr |>
    model(
      snaive = fable::SNAIVE(value),
      rw     = fable::RW(value ~ drift()),
      ets    = fable::ETS(value)
    )
  m_sn <- fit$snaive[[1]]
  m_rw <- fit$rw[[1]]
  m_et <- fit$ets[[1]]
  
  # mean() creates equal-weight model_combination
  cm_mean <- mean(m_sn, m_rw, m_et)
  expect_s3_class(cm_mean$fit, "model_combination")
  expect_equal(cm_mean$fit$weights, rep(1/3, 3), tolerance = 1e-9)
  
  # weighted.mean() normalises weights and stores them
  cm_wm <- weighted.mean(m_sn, w = c(1, 2, 3), m_rw, m_et)
  expect_s3_class(cm_wm$fit, "model_combination")
  expect_equal(cm_wm$fit$weights, c(1, 2, 3) / 6, tolerance = 1e-9)
  
  # sum() uses unit weights (arithmetic sum, not average)
  cm_sum <- sum(m_sn, m_rw, m_et)
  expect_s3_class(cm_sum$fit, "model_combination")
  expect_equal(cm_sum$fit$weights, c(1, 1, 1))
  
  # mean() and combination_ensemble(equal) should yield the same forecast
  fc_mean <- forecast(cm_mean, us_deaths_tr[0,])  # empty new_data trick
  
  mbl_ens <- us_deaths_tr |>
    model(
      cmbn = combination_model(
        fable::SNAIVE(value), fable::RW(value ~ drift()), fable::ETS(value),
        cmbn_args = list(weights = "equal")
      )
    )
  fc_ens <- forecast(mbl_ens, h = 6)
  
  # Weighted mean with equal weights matches equal-weight ensemble
  cm_equal_wm <- weighted.mean(m_sn, w = c(1, 1, 1), m_rw, m_et)
  expect_equal(cm_equal_wm$fit$weights, rep(1/3, 3), tolerance = 1e-9)
})

test_that("Joint combination: weighted.mean weight length mismatch errors", {
  skip_if_not_installed("fable")
  
  fit <- us_deaths_tr |>
    model(snaive = fable::SNAIVE(value), rw = fable::RW(value ~ drift()))
  m_sn <- fit$snaive[[1]]
  m_rw <- fit$rw[[1]]
  
  expect_error(
    weighted.mean(m_sn, w = c(0.5, 0.3, 0.2), m_rw),
    regexp = "length 2"
  )
})

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
