context("setup-data.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths))
USAccDeaths <- as_tsibble(USAccDeaths)

mbl <- USAccDeaths %>%
  model(fable::ETS(value))
fbl <- mbl %>% forecast

mbl_multi <- UKLungDeaths %>%
  model(fable::ETS(value))
fbl_multi <- mbl_multi %>% forecast

# dbl <- USAccDeaths %>%
#   tsibblestats::STL(value)
# 
# dbl_multi <- UKLungDeaths %>%
#   tsibblestats::STL(value)
