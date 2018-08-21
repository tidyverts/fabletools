context("setup-data.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths))
USAccDeaths <- as_tsibble(USAccDeaths)

mbl <- USAccDeaths %>%
  fable::ETS(value)
fbl <- mbl %>% forecast

mbl_multi <- UKLungDeaths %>%
  fable::ETS(value)
fbl_multi <- mbl_multi %>% forecast
