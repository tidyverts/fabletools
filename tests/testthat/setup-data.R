context("setup-data.R")

us_deaths <- as_tsibble(USAccDeaths)
lung_deaths_long <- as_tsibble(cbind(mdeaths, fdeaths))
lung_deaths_wide <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)

mbl <- us_deaths %>%
  model(fable::ETS(value))
fbl <- mbl %>% forecast

mbl_multi <- lung_deaths_long %>%
  model(fable::ETS(value))
fbl_multi <- mbl_multi %>% forecast

mbl_complex <- lung_deaths_long %>% 
  model(ets = fable::ETS(value), lm = fable::TSLM(value ~ trend() + season()))
fbl_complex <- mbl_complex %>% forecast

mbl_mv <- lung_deaths_wide %>% 
  model(var = fable::VAR(vars(mdeaths, fdeaths) ~ fourier(K = 4)))
fbl_mv <- mbl_mv %>% forecast

dcmp <- us_deaths %>%
  feasts::STL(value)

dcmp_multi <- lung_deaths_long %>%
  feasts::STL(value)
