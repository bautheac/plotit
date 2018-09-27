
# FuturesTS ####

#' @rdname plot_term_structure-methods
#' @aliases plot_term_structure,DataHistorical,FuturesTS
#'
#' @examples \dontrun{
#'   library(finRes)
#'   term_structure <- BBG_futures_market(type = 'term structure',
#'     active_contract_tickers = "C A Comdty", start = as.character(Sys.Date() - 365L),
#'     end = as.character(Sys.Date()), TS_positions = 1L:10L,
#'     roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'   plot_term_structure(term_structure, ticker = "C A Comdty")
#' }
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @export
setMethod("plot_term_structure", "FuturesTS", function(object, ticker, frame) {

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

  suppressWarnings(plotly::plot_ly(data, x = ~position, y = ~close, color = ~`open interest`, size = ~volume, frame = ~date,
                                   text = ~paste0("close price:\t", close, "\nopen interest:\t", `open interest`, "\nvolume:\t", volume),
                                   hoverinfo = "text", type = "scatter", mode = "lines+markers", line = list(color = "black", width = 1L)) %>%
                     plotly::layout(title = ticker, xaxis = list(title = ""), yaxis = list(title = "close price"),
                                    legend = list(orientation = "h", xanchor = "center",x = 0.5)) %>%
                     plotly::animation_opts(frame = frame, transition = 0L, redraw = FALSE) %>%
                     plotly::animation_button(x = 1L, xanchor = "right", y = 0L, yanchor = "bottom") %>%
                     plotly::animation_slider(currentvalue = list(prefix = "", font = list(color = "black")))
  )
})



# FundMarket ####

#' @param ticker a scalar vector. Specifies the fund Bloomberg ticker to plot performance for.
#'
#' @rdname plot_performance-methods
#' @aliases plot_performance,DataHistorical,FundMarket
#'
#' @importClassesFrom pullit FundMarket
#' @import BBGsymbols
#'
#' @export
setMethod("plot_performance", "FundMarket", function(object, ticker) {

  if (! all(rlang::is_scalar_character(ticker), ticker %in% object@tickers$ticker))
    stop("The parameter 'ticker' must be supplied as a scalar character vector; one of '", paste(object@tickers$ticker, collapse = "', '"), "'.")

  data(list = c("fields"), package = "BBGsymbols", envir = environment())

  data <- dplyr::filter(object@data, ticker == !! ticker, field %in% c("CUR_MKT_CAP", "EQY_SH_OUT", "FUND_FLOW", "PX_LAST", "PX_VOLUME")) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "fund", book == "market") %>% dplyr::select(symbol, name), by = c("field" = "symbol")) %>%
    dplyr::select(ticker, field = name, date, value) %>%
    dplyr::mutate(field = forcats::as_factor(field))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = date, y = value, colour = field)) +
    ggplot2::geom_line(alpha = 0.6) + ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::facet_wrap(~field, scales = "free") +
    ggthemes::theme_tufte(base_size = 12L) +
    ggplot2::ggtitle(ticker) +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5))

  plotly::ggplotly(p)

})



#' @rdname plot_performance-methods
#' @aliases plot_performance,AssetPricingFactor
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @export
setMethod("plot_performance", "AssetPricingFactor", function(object) {

  data <- apply(dplyr::select(object@returns, tidyselect::matches("long|short|object")) + 1L,
                function(x) cumprod(x), MARGIN = 2L)
  data <- xts::xts(data, order.by = as.Date(object@returns$date))

  if (all(c("long", "short") %in% names(data))) dygraphs::dygraph(data, main = paste(object@name, "factor")) %>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")
  else dygraphs::dygraph(data[, "factor"], main = paste(object@name, "factor"))%>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")

})




#' @rdname plot_positions-methods
#' @aliases plot_positions,AssetPricingFactor
#'
#' @export
setMethod("plot_positions", "AssetPricingFactor", function(object) {
  data <- dplyr::group_by(object@positions, position) %>%
    tidyr::nest() %>%
    dplyr::mutate(proportion = purrr::map(data, function(x) dplyr::group_by(x, name) %>%
                                            dplyr::tally() %>%
                                            dplyr::mutate(n = n / nrow(x))
    )) %>%
    tidyr::unnest(proportion) %>%
    rbind(dplyr::group_by(data, name) %>%
            dplyr::tally() %>%
            dplyr::mutate(position = "factor", n = n / nrow(object@positions))) %>%
    dplyr::rename(proportion = n)

  ggplot2::ggplot(data = data, mapping = ggplot2::aes(name, proportion, fill = name)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(breaks = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~position, ncol = 1L) +
    ggthemes::theme_tufte() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

})

