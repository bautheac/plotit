

plot_performance <- function(object){

  # browser()
  data <- object@returns
  data[1L, grep("long|short|factor", names(data))] <- 1L
  data[-1L, grep("long|short|factor", names(data))] <-
    data[-1L, grep("long|short|factor", names(data))] + 1L
  data <- apply(dplyr::select(data, tidyselect::matches("long|short|factor")),
                function(x) cumprod(x), MARGIN = 2L)
  data <- xts::xts(data, order.by = as.Date(object@returns$date))

  if (all(c("long", "short") %in% names(data)))
    dygraphs::dygraph(data, main = paste(object@name, "factor")) %>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")
  else dygraphs::dygraph(data[, "factor"], main = paste(object@name, "factor")) %>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")

}



plot_positions <- function(object){

  # browser()
  data <- dplyr::group_by(object@positions, position) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      proportion = purrr::map(data, function(x) {
        dplyr::group_by(x, `name`) %>% dplyr::tally() %>%
          dplyr::mutate(proportion = n / nrow(x)) %>% dplyr::select(-n)
      })
    ) %>%
    dplyr::select(position, proportion) %>%
    tidyr::unnest(proportion) %>%
    dplyr::bind_rows(
      dplyr::group_by(object@positions, `name`) %>% dplyr::tally() %>%
        dplyr::mutate(
          position = "factor",
          proportion = n / nrow(object@positions)
        ) %>% dplyr::select(-n)
    )

  ggplot2::ggplot(data = data, mapping = ggplot2::aes(`name`, proportion, fill = `name`)) +
    ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(breaks = NULL) + ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~position, ncol = 1L) + ggthemes::theme_tufte() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

}



