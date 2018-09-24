
[![Travis-CI Build Status](https://travis-ci.org/bautheac/plotit.svg?branch=master)](https://travis-ci.org/bautheac/plotit)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/plotit?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/plotit)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# plotit

Plot methods for various S4 objects ouput by functions in packages that belongs to
  the [finRes](https://bautheac.github.io/finRes/) suite.

## Installation

Install the development version from [github](https://github.com/bautheac/plotit/) with:

``` r
devtools::install_github(repo = "plotit", username = "bautheac")
```

## Example

Market performance plot for an ETF with data retrieved from Bloomberg via [pullit](https://bautheac.github.io/pullit/):

``` r
library(finRes)
fund <- BBG_fund_market(tickers = "SPY US Equity", start = as.character(Sys.Date() - 365L), end = as.character(Sys.Date()))
plot_performance(fund, ticker = "SPY US Equity")
```

