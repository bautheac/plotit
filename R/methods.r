
# FuturesTS ####

#' @rdname plot-methods
#' @aliases plot,FuturesTS
#'
#'
#' @description Plots historical futures term structure data contained in an S4 object
#'   of class \linkS4class{FuturesTS} from the
#'   \href{https://bautheac.github.io/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param ticker a scalar character vector. Active contract Bloomberg ticker to plot the
#'   term structure for.
#'
#' @param frame a scalar integer vector. Animation speed parameter; the lower the faster.
#'
#'
#' @examples \dontrun{
#'
#'   library(finRes)
#'
#'   # pull data from Bloomberg with pullit
#'   term_structure <- pull_futures_market(Bloomberg = T, type = 'term structure',
#'     active_contract_tickers = "C A Comdty", start = as.character(Sys.Date() - 365L),
#'     end = as.character(Sys.Date()), TS_positions = 1L:10L, roll_type = "A",
#'     roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'
#'   # plot futures term structure
#'   plot(term_structure, ticker = "C A Comdty")
#'
#' }
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @export
setMethod("plot", "FuturesTS", function(object, ticker, frame = NULL) {

  if (! all(rlang::is_scalar_character(ticker), ticker %in% object@active_contract_tickers$`active contract ticker`))
    stop("The parameter 'ticker' must be supplied as a scalar character vector; one of '",
         paste(object@active_contract_tickers$`active contract ticker`, collapse = "', '"), "'.")

  data <- dplyr::semi_join(object@data,
                           dplyr::filter(object@term_structure_tickers, `active contract ticker` == !! ticker) %>%
                             dplyr::select(`active contract ticker`, ticker),
                           by = "ticker") %>%
    dplyr::left_join(dplyr::select(object@term_structure_tickers, ticker, position = `TS position`), by = "ticker") %>%
    dplyr::select(position, field, date, value) %>% dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::spread(field, value) %>%
    dplyr::select(date, position, close = PX_LAST, `open interest` = OPEN_INT, volume = PX_VOLUME) %>%
    dplyr::arrange(date, position)

  if (is.null(frame)) frame <- NROW(unique(data$date)) / 100L

  suppressWarnings(
    plotly::plot_ly(data, x = ~position, y = ~close, color = ~`open interest`, size = ~volume, frame = ~date,
                    text = ~paste0("close price:\t", close, "\nopen interest:\t", `open interest`, "\nvolume:\t", volume),
                    hoverinfo = "text", type = "scatter", mode = "lines+markers", line = list(color = "black", width = 1L)) %>%
      plotly::layout(title = ticker, xaxis = list(title = ""), yaxis = list(title = "close price")) %>%
      plotly::animation_opts(frame = frame, transition = 0L, redraw = FALSE) %>%
      plotly::animation_button(x = 1L, xanchor = "right", y = 0L, yanchor = "bottom") %>%
      plotly::animation_slider(currentvalue = list(prefix = "", font = list(color = "black")))
  )

})



# FundMarket ####

#' @rdname plot-methods
#' @aliases plot,FundMarket
#'
#'
#' @description Plots historical market performance indicators for S4 objects
#'   of class \linkS4class{FundMarket} from the
#'   \href{https://bautheac.github.io/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param ticker a scalar vector. Specifies the fund Bloomberg ticker to plot performance for.
#'
#'
#' @examples \dontrun{
#'
#'   library(finRes)
#'
#'   # pull data from Bloomberg with pullit
#'   fund <- pull_fund_market(Bloomberg = T, tickers = "SPY US Equity",
#'     start = as.character(Sys.Date() - 365L), end = as.character(Sys.Date()))
#'
#'   # plot fund performance
#'   plot(fund, ticker = "SPY US Equity")
#'
#' }
#'
#'
#' @importClassesFrom pullit FundMarket
#' @import BBGsymbols
#'
#' @export
setMethod("plot", "FundMarket", function(object, ticker) {

  if (! all(rlang::is_scalar_character(ticker), ticker %in% object@tickers$ticker))
    stop("The parameter 'ticker' must be supplied as a scalar character vector; one of '", paste(object@tickers$ticker, collapse = "', '"), "'.")

  data(list = c("fields"), package = "BBGsymbols", envir = environment())

  dataset <- dplyr::filter(object@data, ticker == !! ticker, field %in% c("CUR_MKT_CAP", "EQY_SH_OUT", "FUND_FLOW", "PX_LAST", "PX_VOLUME")) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "fund", book == "market") %>% dplyr::select(symbol, name), by = c("field" = "symbol")) %>%
    dplyr::select(field = name, date, value) %>%
    dplyr::mutate(field = forcats::as_factor(field), date = as.Date(date), value = as.numeric(value))

  p <- ggplot2::ggplot(data = dataset, ggplot2::aes(x = date, y = value, colour = field, group = 1L)) +
    ggplot2::geom_line(alpha = 0.6) + ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::facet_wrap(~field, scales = "free") +
    ggthemes::theme_tufte(base_size = 12L) +
    ggplot2::ggtitle(ticker) +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5))

  plotly::ggplotly(p)

})




#' @rdname plot-methods
#' @aliases plot,AssetPricingFactor
#'
#'
#' @description Plots historical market performance (type = "performance")
#'   or positions summary (type = "positions") by leg for S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://bautheac.github.io/factorem/}{\pkg{factorem}} package..
#'
#'
#' @param type a scalar character vector. Specifies the type of plot
#'   desired: "performance" or "positions".
#'
#'
#' @examples \dontrun{
#'
#'   library(finRes)
#'
#'   # pull data from Bloomberg via pullit
#'   tickers <- c("C A Comdty", "S A Comdty", "SMA Comdty", "BOA Comdty",
#'       "W A Comdty", "KWA Comdty", "MWA Comdty", "O A Comdty")#'
#'   term_structure <- pull_futures_market(Bloomberg = T, type = 'term structure',
#'     active_contract_tickers = tickers, start = as.character(Sys.Date() - (2L * 365L)),
#'     end = as.character(Sys.Date()), TS_positions = 1L, roll_type = "A", roll_days = 0L,
#'     roll_months = 0L, roll_adjustment = "N")
#'
#'   # construct an asset pricing factor with factorem
#'   factor <- momentum_factor(term_structure)
#'
#'   # plot factor performance
#'   plot(factor, type = "performance")
#'
#'   # plot factor performance
#'   plot(factor, type = "positions")
#'
#' }
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @export
setMethod("plot", "AssetPricingFactor", function(object, type) {

  switch(type,
         performance = plot_performance(object),
         positions = plot_positions(object),
         stop("The parameters 'type' must be supplied as a scalar character vector:
              'performance' or 'positions'.")
         )

})



