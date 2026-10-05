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

test_that("tidy(), glance(), augment() and coef() with mdl_df", {
  skip_if_not_installed("fable")

  grp <- cbind(mbl_complex$ets, mbl_complex$lm)
  td <- tidy(grp)
  expect_equal(names(td), c("ets", "lm"))
  expect_equal(td$ets, tidy(mbl_complex$ets))
  expect_equal(coef(grp), td)

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  mdl_names <- c("ets", "lm", "grp$ets", "grp$lm")
  td_grp <- tidy(mbl_grp)
  expect_equal(unique(td_grp$.model), mdl_names)
  expect_equal(
    dplyr::select(dplyr::filter(td_grp, .model == "grp$lm"), -.model),
    dplyr::select(dplyr::filter(tidy(mbl_complex), .model == "lm"), -.model)
  )
  expect_equal(coef(mbl_grp), td_grp)

  gl_grp <- glance(mbl_grp)
  expect_equal(NROW(gl_grp), 2 * 4)
  expect_equal(unique(gl_grp$.model), mdl_names)

  aug_grp <- augment(mbl_grp)
  expect_s3_class(aug_grp, "tbl_ts")
  expect_equal(unique(aug_grp$.model), mdl_names)
  expect_equal(
    dplyr::filter(aug_grp, .model == "grp$ets")$.fitted,
    dplyr::filter(augment(mbl_complex), .model == "ets")$.fitted
  )
})

test_that("accuracy(), residuals(), fitted(), components() and refit() with mdl_df", {
  skip_if_not_installed("fable")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  mdl_names <- c("ets", "lm", "grp$ets", "grp$lm")

  acc <- accuracy(mbl_grp)
  expect_equal(unique(acc$.model), mdl_names)
  expect_equal(
    dplyr::select(dplyr::filter(acc, .model == "grp$lm"), -.model),
    dplyr::select(dplyr::filter(acc, .model == "lm"), -.model)
  )

  res <- residuals(mbl_grp)
  expect_equal(unique(res$.model), mdl_names)
  expect_equal(
    dplyr::filter(res, .model == "grp$ets")$.resid,
    dplyr::filter(res, .model == "ets")$.resid
  )
  fits <- fitted(mbl_grp)
  expect_equal(
    dplyr::filter(fits, .model == "grp$lm")$.fitted,
    dplyr::filter(fits, .model == "lm")$.fitted
  )

  cmp <- components(mbl_grp %>% mutate(grp = cbind(ets, ets2 = ets)) %>% select(key, grp))
  expect_s3_class(cmp, "dcmp_ts")
  expect_equal(unique(cmp$.model), c("grp$ets", "grp$ets2"))

  # Modifying generics return the model group with its models replaced
  refitted <- refit(mbl_grp, lung_deaths_long)
  expect_true(is_mable(refitted))
  expect_true(is_mdl_df(refitted$grp))
  expect_equal(
    dplyr::select(dplyr::filter(tidy(refitted), .model == "grp$lm"), -.model),
    dplyr::select(dplyr::filter(tidy(refitted), .model == "lm"), -.model)
  )
})
