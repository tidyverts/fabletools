context("test-mdl_df")

test_that("cbind() combines model columns into a mdl_df", {
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

  # Unnamed model data frames are extended, named ones are nested
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

  expect_true(is_mable(rbind(mbl_complex[1,], mbl_complex[2,])))
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

  # Modifying generics return the model data frame with its models replaced
  refitted <- refit(mbl_grp, lung_deaths_long)
  expect_true(is_mable(refitted))
  expect_true(is_mdl_df(refitted$grp))
  expect_equal(
    dplyr::select(dplyr::filter(tidy(refitted), .model == "grp$lm"), -.model),
    dplyr::select(dplyr::filter(tidy(refitted), .model == "lm"), -.model)
  )
})

test_that("report() and equation() with a single model in a model data frame", {
  skip_if_not_installed("fable")

  mbl_one <- mbl_complex %>%
    dplyr::filter(key == "mdeaths") %>%
    transmute(grp = cbind(ets = cbind(ets)))
  expect_equal(
    capture.output(report(mbl_one)),
    capture.output(report(dplyr::filter(mbl_complex, key == "mdeaths")$ets[[1]]))
  )
  # The equation is requested from the model itself
  expect_error(equation(mbl_one), "class \"ETS\"")

  mbl_two <- mbl_complex %>%
    dplyr::filter(key == "mdeaths") %>%
    transmute(grp = cbind(ets, lm))
  expect_warning(report(mbl_two), "only supported for individual models")
  expect_error(equation(mbl_two), "only supported for individual models")
})

test_that("equation() requires a model in a mable without models", {
  skip_if_not_installed("fable")

  expect_error(
    equation(select(mbl_complex[0, ], key, ets)),
    "only supported for individual models"
  )
})

test_that("pivot_longer() and gather() unpack model data frames", {
  skip_if_not_installed("fable")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  mdl_names <- c("ets", "lm", "grp$ets", "grp$lm")

  long <- tidyr::pivot_longer(mbl_grp, -key, names_to = ".model", values_to = "fit")
  expect_true(is_mable(long))
  expect_equal(unique(long$.model), mdl_names)
  expect_equal(mable_vars(long), "fit")
  expect_s3_class(long$fit, "mdl_lst")

  # Unselected model data frames are kept
  long <- tidyr::pivot_longer(mbl_grp, c(ets, lm))
  expect_equal(mable_vars(long), c("grp", "value"))
  expect_true(is_mdl_df(long$grp))

  gathered <- tidyr::gather(mbl_grp, ".model", "fit", ets, grp)
  expect_true(is_mable(gathered))
  expect_equal(unique(gathered$.model), c("ets", "grp$ets", "grp$lm"))
  expect_equal(mable_vars(gathered), c("lm", "fit"))
  expect_equal(unique(tidyr::gather(mbl_grp, ".model", "fit", -key)$.model), mdl_names)
})

test_that("model data frame labels must be unique", {
  skip_if_not_installed("fable")

  # A model column named like a model of a model data frame
  mbl_dup <- mutate(mbl_complex, grp = cbind(ets, lm), `grp$ets` = lm)
  msg <- "`grp\\$ets` is both a column and a model of `grp`"
  expect_error(forecast(mbl_dup, h = 1), msg)
  expect_error(accuracy(mbl_dup), msg)
  expect_error(glance(mbl_dup), msg)
  expect_error(tidy(mbl_dup), msg)
  expect_error(augment(mbl_dup), msg)
  expect_error(tidyr::pivot_longer(mbl_dup, -key), msg)

  # A model named like a model of a nested model data frame
  grp <- cbind(a = cbind(b = mbl_complex$ets), `a$b` = mbl_complex$lm)
  mbl_dup <- mutate(mbl_complex, grp = grp)
  expect_error(glance(mbl_dup), "`grp\\$a\\$b` labels more than one model")
})

test_that("modifiers applied to model data frames", {
  skip_if_not_installed("fable")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))

  # Independent modifiers are applied to each model of the model data frame
  sim <- mutate(mbl_grp, grp = simulate_iid(grp, times = 10))
  expect_true(is_mdl_df(sim$grp))
  expect_s3_class(sim$grp$ets[[1]], "mdl_ts_sim")
  expect_s3_class(sim$grp$lm[[1]], "mdl_ts_sim")
  fc <- forecast(sim, h = 2)
  expect_true(all(dist_types(dplyr::filter(fc, .model == "grp$lm")$value) == "dist_sample"))

  cnf <- mutate(mbl_grp, grp = conformal_scp(grp))
  expect_s3_class(cnf$grp$ets[[1]], "mdl_conformal_scp")
  expect_s3_class(cnf$grp$lm[[1]], "mdl_conformal_scp")

  boot <- mutate(mbl_grp, grp = cbind(ets = bootstrap_iid(ets, times = 10), lm))
  expect_s3_class(boot$grp$ets, "mdl_lst_bootstrap_iid")
  expect_equal(NROW(forecast(boot, h = 2)), 2 * 4 * 2)

  # Reconciliation applies to a column of models across the hierarchy
  expect_error(mutate(mbl_grp, grp = reconcile_bu(grp)), "can't be applied to a model data frame")
  expect_error(mutate(mbl_grp, grp = min_trace(grp)), "can't be applied to a model data frame")
  expect_error(reconcile_mint(mbl_complex), "must be applied to a column of models")

  # Modifiers can't be applied to a mable
  expect_error(simulate_iid(mbl_complex), "not to a mable")
})

test_that("bootstrapping a model data frame samples jointly across all of its models", {
  skip_if_not_installed("fable")

  mbl_grp <- mbl_complex %>%
    mutate(grp = cbind(ets, lm))
  # The time points drawn for every model of the model data frame
  group_draws <- function(grp, ...) {
    grp <- fabletools:::bootstrap_joint(grp, NULL, h = 3, times = 2, ...)
    lapply(fabletools:::model_list(grp), attr, "bootstrap_draws")
  }

  for (modifier in list(bootstrap_iid, bootstrap_block)) {
    fit <- mutate(mbl_grp, grp = modifier(grp, times = 10))
    expect_s3_class(fit$grp, "mdl_df")
    expect_s3_class(fit$grp$ets[[1]], "mdl_ts_sim")
    draws <- group_draws(fit$grp, block_size = fit$grp %@% "block_size")
    expect_length(draws, 4)
    expect_length(draws[[1]], 6)
    expect_true(all(vapply(draws, identical, logical(1L), draws[[1]])))

    fc <- forecast(fit, h = 2)
    expect_true(all(dist_types(dplyr::filter(fc, .model %in% c("grp$ets", "grp$lm"))$value) == "dist_sample"))
  }

  # Identical models of identical series give identical paths
  two_id <- dplyr::bind_rows(
    dplyr::mutate(as_tibble(us_deaths_tr), id = "A"),
    dplyr::mutate(as_tibble(us_deaths_tr), id = "B")
  ) %>% as_tsibble(index = index, key = id)
  fit <- two_id %>%
    model(a = fable::ETS(value ~ error("A") + trend("N") + season("A"))) %>%
    mutate(b = a) %>%
    transmute(grp = bootstrap_iid(cbind(a, b), times = 5))
  set.seed(1)
  gen <- generate(fit, h = 3, times = 5)
  sims <- split(gen$.sim, list(gen$id, gen$.model))
  expect_true(all(vapply(sims, identical, logical(1L), sims[[1]])))

  # The model data frame's draws are used by jointly bootstrapped and reconciled columns
  # within it
  fit <- lung_deaths_long_tr %>%
    aggregate_key(key, value = sum(value)) %>%
    model(
      ses = fable::ETS(value ~ error("A") + trend("N") + season("N")),
      snaive = fable::SNAIVE(value)
    ) %>%
    transmute(grp = bootstrap_iid(cbind(ses = reconcile_mint(ses), snaive = bootstrap_block(snaive)), times = 10))
  expect_s3_class(fit$grp$ses, "lst_mint_mdl")
  expect_s3_class(fit$grp$snaive, "mdl_lst_bootstrap_block")
  # The seasonal naive residuals start a year later
  expect_warning(draws <- group_draws(fit$grp), "overlapping period")
  expect_length(draws, 6)
  expect_true(all(vapply(draws, identical, logical(1L), draws[[1]])))
  expect_warning(fc <- forecast(fit, h = 3), "overlapping period")
  fc <- dplyr::filter(fc, .model == "grp$ses")
  tot <- fc$.mean[is_aggregated(fc$key)]
  btm <- tapply(fc$.mean[!is_aggregated(fc$key)], fc$index[!is_aggregated(fc$key)], sum)
  expect_equal(tot, as.numeric(btm))

  # Model data frames modified differently can't be combined
  boot <- mutate(mbl_grp, grp = bootstrap_iid(grp))
  expect_s3_class(vctrs::vec_rbind(boot$grp[1, ], boot$grp[2, ]), "mdl_df_bootstrap_iid")
  expect_error(vctrs::vec_rbind(boot$grp[1, ], mbl_grp$grp[2, ]), "different modifiers")
})
