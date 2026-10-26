context("test-plot.R")

test_that("fabletools' plot methods are registered without ggtime", {
  skip_if_not_installed("ggplot2")
  s3_table <- asNamespace("ggplot2")[[".__S3MethodsTable__."]]
  methods <- c(
    "autoplot.tbl_ts", "autoplot.fbl_ts", "autoplot.dcmp_ts",
    "autolayer.tbl_ts", "autolayer.fbl_ts", "fortify.fbl_ts"
  )
  # Restore the previously registered methods afterwards
  registered <- mget(methods, envir = s3_table, ifnotfound = list(NULL))
  on.exit(for (m in methods) {
    if (is.null(registered[[m]])) {
      suppressWarnings(rm(list = m, envir = s3_table))
    } else {
      assign(m, registered[[m]], envir = s3_table)
    }
  })

  register_plot_methods(NA_character_)
  expect_identical(s3_table[["autolayer.tbl_ts"]], autolayer.tbl_ts)
  expect_identical(s3_table[["autolayer.fbl_ts"]], autolayer.fbl_ts)
  expect_identical(s3_table[["autoplot.tbl_ts"]], autoplot.tbl_ts)
})
