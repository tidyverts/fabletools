context("test-mdl_df")

test_that("cbind() groups model columns into a mdl_df", {
  skip_if_not_installed("fable")

  grp <- cbind(mbl_complex$ets, mbl_complex$lm)
  expect_s3_class(grp, "mdl_df")
  expect_false(is_mable(grp))
  expect_equal(names(grp), c("ets", "lm"))
  expect_equal(NROW(grp), 2)
  expect_equal(mable_vars(grp), c("ets", "lm"))
  expect_equal(response_vars(grp), "value")
  expect_equal(
    model_sum(grp),
    paste(map_chr(mbl_complex$ets, model_sum), "+", map_chr(mbl_complex$lm, model_sum))
  )

  # Symbols and explicit names
  ets <- mbl_complex$ets
  expect_equal(names(cbind(ets, trend = mbl_complex$lm)), c("ets", "trend"))

  # Unnamed groups are extended, named groups are nested
  expect_equal(names(cbind(grp, ets2 = ets)), c("ets", "lm", "ets2"))
  nested <- cbind(grp = grp, ets2 = ets)
  expect_equal(names(nested), c("grp", "ets2"))
  expect_s3_class(nested$grp, "mdl_df")

  expect_error(cbind(ets, ets), "unique names")
  expect_error(cbind(ets, 1:2), "Only model columns")
  expect_error(cbind(ets, other = mbl$ets), "same number of models")
})

test_that("rbind() stacks mdl_df rows", {
  skip_if_not_installed("fable")

  grp <- cbind(mbl_complex$ets, mbl_complex$lm)
  stacked <- rbind(grp, grp)
  expect_s3_class(stacked, "mdl_df")
  expect_equal(NROW(stacked), 4)
  expect_equal(names(stacked), c("ets", "lm"))
  expect_s3_class(stacked$ets, "mdl_lst")

  # Matching model columns are aligned by name
  expect_equal(names(rbind(grp, grp[c("lm", "ets")])), c("ets", "lm"))
  expect_error(rbind(grp, grp["ets"]), "different model columns")

  expect_s3_class(vec_slice(grp, 1), "mdl_df")
})

test_that("mables with trailing mdl_df class are unaffected by mdl_df methods", {
  skip_if_not_installed("fable")

  expect_true(is_mable(rbind(mbl_complex, mbl_complex)))
  expect_equal(model_sum(mbl_complex), model_sum.default(mbl_complex))
})

test_that("mdl_df columns in a mable", {
  skip_if_not_installed("fable")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  expect_true(is_mable(mbl_grp))
  expect_equal(mable_vars(mbl_grp), c("ets", "lm", "grp"))
  expect_output(print(mbl_grp), "grp\\$ets")

  mbl_grp$grp2 <- cbind(mbl_grp$lm, mbl_grp$ets)
  expect_equal(mable_vars(mbl_grp), c("ets", "lm", "grp", "grp2"))

  expect_equal(mable_vars(select(mbl_grp, key, grp)), "grp")
  expect_equal(NROW(filter(mbl_grp, key == "mdeaths")$grp), 1)
})

test_that("forecast() and generate() with mdl_df", {
  skip_if_not_installed("fable")

  grp <- cbind(mbl_complex$ets, mbl_complex$lm)
  fc <- forecast(grp, h = 2)
  expect_equal(names(fc), c("ets", "lm"))
  expect_equal(NROW(fc), 2)
  expect_s3_class(fc$ets[[1]], "fbl_ts")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  fbl_grp <- forecast(mbl_grp, h = 12)
  expect_equal(unique(fbl_grp$.model), c("ets", "lm", "grp$ets", "grp$lm"))
  expect_equal(
    dplyr::filter(fbl_grp, .model == "grp$ets")$value,
    dplyr::filter(fbl_complex, .model == "ets")$value
  )

  sim <- generate(mbl_grp, h = 12, times = 2)
  expect_equal(unique(sim$.model), c("ets", "lm", "grp$ets", "grp$lm"))
  expect_equal(NROW(sim), 12 * 2 * 4 * 2)
})
