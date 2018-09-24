#' Plots futures term structure over time.
#'
#' @description Plots historical futures term structure data contained in an S4 object
#'   of class \linkS4class{FuturesTS} from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#' @param ticker a scalar character vector. Active contract Bloomberg ticker to plot the term structure for.
#' @param frame a scalar integer vector. Animation speed parameter; the lower the faster.
#'
#' @docType methods
#' @rdname plot_term_structure-methods
#'
#' @export
setGeneric("plot_term_structure", function(object, ticker, frame) standardGeneric("plot_term_structure"))



#' Plots market performance for various S4 objects ouput of packages that belong to the
#'   \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#' @description Plots historical market performance inficators for various S4 objects
#'   from packages belonging to the \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} and childs or \linkS4class{AssetPricingFactor}.
#' @param ... extra parameters for \code{object} related methods.
#'
#' @examples \dontrun{
#'   library(finRes)
#'
#'   # funds ####
#'   ## pull data from Bloomberg with pullit ####
#'   fund <- BBG_fund_market(tickers = "SPY US Equity",
#'     start = as.character(Sys.Date() - 365L), end = as.character(Sys.Date()))
#'   ## plot fund performance ####
#'   plot_performance(fund, ticker = "SPY US Equity")
#'
#'   # asset pricing factors ####
#'   ## pull data from Bloomberg via pullit ####
#'   term_structure <- BBG_futures_market(type = 'term structure',
#'     active_contract_tickers = c("C A Comdty", "S A Comdty", "SMA Comdty", "BOA Comdty",
#'       "W A Comdty", "KWA Comdty", "MWA Comdty", "O A Comdty",
#'       "CHEA Comdty", "V6A Comdty", "FCA Comdty", "LCA Comdty", "LHA Comdty",
#'       "SBA Comdty", "CCA Comdty", "CTA Comdty", "KCA Comdty", "JOA Comdty", "LBA Comdty",
#'       "GCA Comdty", "SIA Comdty", "PAA Comdty", "PLA Comdty", "LAA Comdty", "LLA Comdty",
#'       "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty",
#'       "CLA Comdty", "COA Comdty", "DLA Comdty", "NGA Comdty", "HOA Comdty", "XBA Comdty"),
#'     start = as.character(Sys.Date() - (2L * 365L)), end = as.character(Sys.Date()),
#'     TS_positions = 1L, roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'   ## construct an asset pricing factor with factorem ####
#'   factor <- momentum_factor(term_structure)
#'   ## plot factor performance ####
#'   plot_performance(factor)
#' }
#'
#'
#' @docType methods
#' @rdname plot_performance-methods
#'
#' @export
setGeneric("plot_performance", function(object, ...) standardGeneric("plot_performance"))



#' Plots factor positions summary by legs
#'
#' @description Plots a summary of a factor's positions by legs.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @docType methods
#' @rdname plot_positions-methods
#'
#' @export
setGeneric("plot_positions", function(object) standardGeneric("plot_positions"))
