context("setup-data.R")

UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths))
USAccDeaths <- as_tsibble(USAccDeaths)