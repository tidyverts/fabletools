context("test-graphics")

library(ggplot2)
test_that("autoplot.tbl_ts()", {
  expect_message(
    p <- autoplot(us_deaths),
    "Plot variable not specified, automatically selected \\`.vars = value\\`"
  )
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    us_deaths$value
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "value")
  )
  
  lambda <- 0
  p <- autoplot(us_deaths, box_cox(value, lambda))
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    log(us_deaths$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "box_cox(value, lambda)")
  )
  
  p <- autoplot(lung_deaths_long, value)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    lung_deaths_long$value[c(73:144, 1:72)]
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "value")
  )
  
  p <- autoplot(lung_deaths_wide, vars(mdeaths, fdeaths))
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(lung_deaths_wide$mdeaths, lung_deaths_wide$fdeaths)
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = NULL)
  )
  
  p <- autoplot(lung_deaths_wide, vars(mdeaths, log(fdeaths)))
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(lung_deaths_wide$mdeaths, log(lung_deaths_wide$fdeaths))
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = NULL)
  )
})


test_that("autolayer.tbl_ts()", {
  expect_message(
    p <- ggplot() + autolayer(us_deaths),
    "Plot variable not specified, automatically selected \\`.vars = value\\`"
  )
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    us_deaths$value
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  lambda <- 0
  p <- ggplot() + autolayer(us_deaths, box_cox(value, lambda))
  expect_silent(print(p))
  expect_equal(
    ggplot2::layer_data(p)$y,
    log(us_deaths$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "box_cox(value, lambda)")
  )
  
  p <- ggplot() + autolayer(lung_deaths_long, value)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    lung_deaths_long$value[c(73:144, 1:72)]
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- ggplot() + autolayer(lung_deaths_wide, vars(mdeaths, fdeaths))
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(lung_deaths_wide$mdeaths, lung_deaths_wide$fdeaths)
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$group)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- ggplot() + autolayer(lung_deaths_wide, vars(log(mdeaths), log(fdeaths)))
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(log(lung_deaths_wide$mdeaths), log(lung_deaths_wide$fdeaths))
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$group)),
    rep(72, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("autoplot.fbl_ts()", {
  skip_if_not_installed("fable")
  
  p <- autoplot(fbl)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p, 3)$y,
    mean(fbl$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- autoplot(fbl, us_deaths)
  expect_silent(print(p))

  expect_equal(
    ggplot2::layer_data(p, 3)$y,
    mean(fbl$value)
  )
  expect_equal(
    ggplot2::layer_data(p, 5)$y,
    us_deaths$value
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- autoplot(fbl_complex, lung_deaths_long, level = 95)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p, 3)$y,
    mean(fbl_complex$value[c(1:24, 25:48)])
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p, 3)$colour)),
    rep(24, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- autoplot(fbl_mv, lung_deaths_wide, level = 80)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p, 3)$y,
    c(fbl_mv$.mean_fdeaths, fbl_mv$.mean_mdeaths)
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(12, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = NULL)
  )
})

test_that("autolayer.fbl_ts()", {
  skip_if_not_installed("fable")
  
  p <- autoplot(us_deaths, value) + autolayer(fbl)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p,4)$y,
    mean(fbl$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "value")
  )
  
  p <- autoplot(lung_deaths_long, value) + autolayer(fbl_complex, level = 95)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p,4)$y,
    mean(fbl_complex$value[c(1:12, 25:36, 13:24, 37:48)])
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p,4)$colour)),
    rep(12, 4)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "value")
  )
  
  p <- autoplot(lung_deaths_wide, vars(mdeaths, fdeaths)) + autolayer(fbl_mv, level = 80)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p,4)$y,
    c(fbl_mv$.mean_fdeaths, fbl_mv$.mean_mdeaths)
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p,2)$PANEL)),
    rep(12, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = NULL)
  )
})

test_that("autoplot_dcmp_ts()", {
  skip_if_not_installed("feasts")
  
  p <- autoplot(dcmp)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(dcmp$value, dcmp$trend, dcmp$season_year, dcmp$remainder)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("title", "subtitle", "x", "y")],
    list(
      title = "STL decomposition",
      subtitle = "value = trend + season_year + remainder",
      x = "index", y = NULL
    )
  )
  
  p <- autoplot(dcmp_multi)
  expect_silent(print(p))
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    c(dcmp_multi$value, dcmp_multi$trend, dcmp_multi$season_year, dcmp_multi$remainder)
  )
  expect_equivalent(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(288, 2)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("title", "subtitle", "x", "y")],
    list(
      title = "STL decomposition",
      subtitle = "value = trend + season_year + remainder",
      x = "index", y = NULL
    )
  )
})