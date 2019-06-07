context("test-graphics")

test_that("autoplot.tbl_ts()", {
  expect_message(
    p <- autoplot(us_deaths),
    "Plot variable not specified, automatically selected \\`.vars = value\\`"
  )
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    us_deaths$value
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "value")
  )
  
  p <- autoplot(us_deaths, log(value))
  expect_equal(
    ggplot2::layer_data(p)$y,
    log(us_deaths$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index [1M]", y = "log(value)")
  )
  
  p <- autoplot(lung_deaths_long, value)
  
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


test_that("autoplot.tbl_ts()", {
  expect_message(
    p <- ggplot() + autolayer(us_deaths),
    "Plot variable not specified, automatically selected \\`.vars = value\\`"
  )
  
  expect_equal(
    ggplot2::layer_data(p)$y,
    us_deaths$value
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
  
  p <- ggplot() + autolayer(us_deaths, log(value))
  expect_equal(
    ggplot2::layer_data(p)$y,
    log(us_deaths$value)
  )
  
  p_built <- ggplot2::ggplot_build(p)
  
  expect_identical(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "log(value)")
  )
  
  p <- ggplot() + autolayer(lung_deaths_long, value)
  
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
