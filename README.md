plotit
================
2018-11-15

<style> body {text-align: justify} </style>

[![Travis-CI Build
Status](https://travis-ci.org/bautheac/plotit.svg?branch=master)](https://travis-ci.org/bautheac/plotit)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/bautheac/plotit?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/plotit)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## plotit

[plotit](https://bautheac.github.io/plotit/) belongs to the
[finRes](https://bautheac.github.io/finRes/) suite where it provides
visualization solutions for packages including, at the time of writing,
[pullit](https://bautheac.github.io/pullit/) and
[factorem](https://bautheac.github.io/factorem/). Install the
development version from github with
`devtools::install_github("bautheac/plotit")`.

## pullit

plotit provides plot methods for a number of pullit objects including,
at the time of writing, futures term structure (4class{FuturesTS}) and
fund market (4class{FundMarket}) objects.

### futures term structure

Plot a futures series term structure dynamics with:

``` r
library(plotit); library(pullit); library(lubridate)

end <- Sys.Date(); start <- end - years(2L)
tickers <- c("C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
             "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", 
             "KCA Comdty", "KWA Comdty", "LBA Comdty", "LCA Comdty", 
             "LHA Comdty", "NGA Comdty", "O A Comdty", "PAA Comdty", 
              "S A Comdty", "SIA Comdty", "W A Comdty", "XBA Comdty")

futures_TS <- BBG_futures_market(type = "term structure", active_contract_tickers = tickers, 
                                 start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, 
                                 roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

plot_term_structure(object = futures_TS, ticker = "C A Comdty")
```

### fund market

Plot historical fund performance with:

``` r
tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")

fund_market <- BBG_fund_market(fund_tickers, start, end, verbose = FALSE)

plot_performance(object = fund_market, ticker = "GLD US Equity")
```

## factorem

### performance overview

``` r
ranking_period = 1L

futures_TS <- OI_nearby_factor(data = futures_TS, ranking_period = ranking_period)

plot_performance(futures_TS)
```

### positions overview

``` r
plot_positions(futures_TS)
```
