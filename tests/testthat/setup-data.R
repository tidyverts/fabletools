context("setup-data.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths))
USAccDeaths <- as_tsibble(USAccDeaths)

mbl <- USAccDeaths %>%
  model(fable::ETS(value))
fbl <- mbl %>% forecast

mbl_multi <- UKLungDeaths %>%
  model(fable::ETS(value))
fbl_multi <- mbl_multi %>% forecast

mbl_complex <- UKLungDeaths %>% 
  model(ets = fable::ETS(value), lm = fable::TSLM(value ~ trend() + season()))
fbl_complex <- mbl_complex %>% forecast

# dbl <- USAccDeaths %>%
#   tsibblestats::STL(value)
# 
# dbl_multi <- UKLungDeaths %>%
#   tsibblestats::STL(value)
