context("test-legacy")

# The classes of objects saved with fabletools <= 0.8.0
as_legacy_mable <- function(x, class = c("mdl_df", "mbl_df")){
  class(x) <- c(class, "tbl_df", "tbl", "data.frame")
  x
}
as_legacy_fable <- function(x){
  class(x) <- setdiff(class(x), "fbl_df")
  x
}

test_that("mables saved with fabletools <= 0.8.0 are used as mables", {
  skip_if_not_installed("fable")

  fit <- model(lung_deaths_long_tr, ets = fable::ETS(value), lm = fable::TSLM(value ~ trend()))
  for (cls in list(c("mdl_df", "mbl_df"), "mdl_df")) {
    old <- as_legacy_mable(fit, cls)
    expect_true(is_legacy_mable(old))
    expect_false(is_mdl_df(old))
    expect_identical(class(upgrade_mable(old)), class(fit))

    expect_equal(mable_vars(old), mable_vars(fit))
    expect_equal(response_vars(old), response_vars(fit))
    expect_output(print(old), "A mable")
    expect_equal(forecast(old, h = 3), forecast(fit, h = 3))
    expect_equal(glance(old), glance(fit))
    expect_equal(accuracy(old), accuracy(fit))
    expect_equal(tidy(old), tidy(fit))
    expect_equal(augment(old), augment(fit))
    expect_true(is_mable(refit(old, lung_deaths_long)))
  }

  old <- as_legacy_mable(fit)
  expect_true(is_mable(dplyr::mutate(old, ets2 = ets)))
  expect_equal(mable_vars(dplyr::select(old, key, lm)), "lm")
})

test_that("model specifications saved with fabletools <= 0.8.0 can be refitted", {
  skip_if_not_installed("fable")

  fit <- model(us_deaths_tr, ets = fable::ETS(value))
  spec <- fit$ets[[1]]$model
  class(spec) <- c("mdl_defn", "R6")
  old <- as_legacy_mable(fit)
  expect_s3_class(refit(old, us_deaths)$ets[[1]]$fit, "ETS")
})

test_that("model groups aren't treated as saved mables", {
  skip_if_not_installed("fable")

  grp <- cbind(mbl_complex$ets, mbl_complex$lm)
  expect_false(is_legacy_mable(grp))
  expect_true(is_mdl_df(grp))
  expect_identical(upgrade_mable(grp), grp)
  expect_equal(mable_vars(grp), c("ets", "lm"))

  fit <- dplyr::mutate(mbl_complex, grp = cbind(ets, lm))
  fc <- forecast(dplyr::select(fit, key, grp), h = 3)
  expect_setequal(unique(fc$.model), c("grp$ets", "grp$lm"))
})

test_that("fables saved with fabletools <= 0.8.0 are used as fables", {
  skip_if_not_installed("fable")

  old <- as_legacy_fable(fbl_complex)
  expect_true(is_fable(old))
  expect_s3_class(upgrade_fable(old), "fbl_df")
  expect_equal(response_vars(old), response_vars(fbl_complex))
  expect_equal(distribution_var(old), distribution_var(fbl_complex))
  expect_equal(
    accuracy(old, lung_deaths_long),
    accuracy(fbl_complex, lung_deaths_long)
  )
  expect_equal(hilo(old), hilo(fbl_complex))
  expect_s3_class(dplyr::filter(old, .model == "ets"), "fbl_df")
  expect_s3_class(dplyr::select(old, -.mean), "fbl_df")
  expect_s3_class(dplyr::mutate(old, .mean = .mean + 1), "fbl_df")
  expect_s3_class(dplyr::group_by(old, .model), "grouped_fbl")
  expect_s3_class(dplyr::group_by(old, .model, .drop = FALSE), "grouped_fbl")
  expect_s3_class(tibble::as_tibble(old), "tbl_df")
})

test_that("fables saved with fabletools <= 0.8.0 forward named tidy-eval arguments", {
  skip_if_not_installed("fable")

  old <- as_legacy_fable(fbl_complex)
  expect_equal(
    names(dplyr::relocate(old, .mean, .before = key)),
    names(dplyr::relocate(fbl_complex, .mean, .before = key))
  )
  # Arguments forwarded through dots of another function
  relocate_fc <- function(x, ...) dplyr::relocate(x, ...)
  expect_equal(
    names(relocate_fc(old, .model, .after = .mean)),
    names(dplyr::relocate(fbl_complex, .model, .after = .mean))
  )
  # Standard arguments are evaluated in the caller's environment
  hilo_level <- function(level) hilo(old, level = level)
  expect_equal(hilo_level(90), hilo(fbl_complex, level = 90))
})
