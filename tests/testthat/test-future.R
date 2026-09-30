test_that("use_future() depends on the active plan, not attachment (#363, #420)", {
  skip_if_not_installed("future")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  future::plan(future::sequential)
  expect_false(use_future())

  future::plan(future::multisession, workers = 2)
  expect_true(use_future())

  future::plan(future::sequential)
  expect_false(use_future())
})
