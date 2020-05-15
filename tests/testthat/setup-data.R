context("setup-data.R")

us_deaths <- as_tsibble(USAccDeaths)
us_deaths_tr <- dplyr::filter(us_deaths, index < tsibble::yearmonth("1978 Jan"))
lung_deaths_long <- as_tsibble(cbind(mdeaths, fdeaths))
lung_deaths_long_tr <- dplyr::filter(lung_deaths_long, index < tsibble::yearmonth("1979 Jan"))
lung_deaths_wide <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)
lung_deaths_wide_tr <- dplyr::filter(lung_deaths_wide, index < tsibble::yearmonth("1979 Jan"))

if (requireNamespace("fable", quietly = TRUE)) {
mbl <- us_deaths_tr %>%
  model(ets = fable::ETS(value))
fbl <- mbl %>% forecast(h = 12)

mbl_multi <- lung_deaths_long_tr %>%
  model(ets = fable::ETS(value))
fbl_multi <- mbl_multi %>% forecast(h = 12)

mbl_complex <- lung_deaths_long_tr %>% 
  model(ets = fable::ETS(value), lm = fable::TSLM(value ~ trend() + season()))
fbl_complex <- mbl_complex %>% forecast(h = 12)

mbl_mv <- lung_deaths_wide_tr %>%
  model(var = fable::VAR(vars(mdeaths, fdeaths) ~ fourier(K = 4)))
fbl_mv <- mbl_mv %>% forecast(h = 12)
}

if (requireNamespace("feasts", quietly = TRUE)) {
dcmp <- us_deaths %>%
  model(feasts::STL(value)) %>% 
  components()

dcmp_multi <- lung_deaths_long %>%
  model(feasts::STL(value)) %>% 
  components()
}
