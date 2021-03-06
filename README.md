plotit\!
================

<style> body {text-align: justify} </style>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[plotit](https://bautheac.github.io/plotit/) belongs to the
[finRes](https://bautheac.github.io/finRes/) suite where it provides
visualization solutions for packages including, at the time of writing,
[pullit](https://bautheac.github.io/pullit/) and
[factorem](https://bautheac.github.io/factorem/).  
Install the development version from github with
`devtools::install_github("bautheac/plotit")`.

## pullit

[plotit](https://bautheac.github.io/plotit/) provides plot methods for a
number of [pullit](https://bautheac.github.io/pullit/) objects
including, at the time of writing, futures term structure (FuturesTS)
and fund market (FundMarket) objects.

### Futures term structure

Plot a futures series term structure dynamics with:

``` r
library(plotit); library(pullit); library(factorem); library(lubridate)

start <- "2016-01-01"; end <- "2017-12-31"
ticker <- "C A Comdty"

data <- pull_futures_market(
  source = "storethat", type = "term structure", active_contract_tickers = ticker,
  start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
  roll_months = 0L, roll_adjustment = "N", verbose = FALSE, file = path
  )

plot(object = data, ticker = ticker)
```

### Fund market

Plot historical fund performance with:

``` r
ticker <- "SPY US Equity"

data <- pull_fund_market(
  source = "storethat", ticker, start, end, verbose = FALSE, file = path
  )

plot(object = data, ticker = ticker)
```

## factorem

Similarly [plotit](https://bautheac.github.io/plotit/) provides plot
methods for [factorem](https://bautheac.github.io/factorem/) factor
objects (AssetPricingFactor).

### Performance overview

Plot historical factor performance with:

``` r
tickers <- c(
  "LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
  "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
  "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity"
  )

data <- pull_equity_market(
  source = "storethat", tickers = tickers, start = start, end = end, 
  verbose = F, file = path
  )

ranking_period <- 1L

factor <- factorem(
  name = "momentum", data = pullit::get_data(data),
  sort_levels = FALSE, weighted = FALSE, ranking_period = ranking_period
  )

plot(factor, type = "performance")
```

### Positions overview

Plot factor positions statistics with:

``` r
plot(factor, type = "positions")
```
