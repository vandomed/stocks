#' Plot Investment Growth
#'
#' Useful for comparing the performance of several investments, over their full
#' histories or mutual lifetimes.
#'
#'
#' @param prices Data frame with one column of prices for each investment and a
#' date variable named Date.
#' @param tickers Character vector of ticker symbols that Yahoo! Finance
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param gains Data frame with one column of gains for each investment and a
#' date variable named Date.
#' @param initial Numeric value specifying value to scale initial prices to.
#' @param plotly Logical value for whether to convert the
#' \code{\link[ggplot2]{ggplot}} to a \code{\link[plotly]{plotly}} object
#' internally.
#' @param title Character string.
#' @param base_size Numeric value to pass to
#' \code{\link[ggplot2:ggtheme]{theme_gray}}.
#' @param tooltip_size Numeric value to pass to \code{\link[plotly]{style}}.
#' @param point_size Numeric value to pass to \code{\link[ggplot2]{geom_point}}.
#' @param line_size Numeric value to pass to
#' \code{\link[ggplot2:geom_path]{geom_line}}.
#' @param ticklabel_size Numeric value to pass to \code{\link[ggplot2]{theme}}.
#' @param legend_position Character string to pass to \code{\link[ggplot2]{theme}}.
#' @param return Character string specifying what to return. Choices are
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#'
#'
#' @return
#' Depending on \code{return} and \code{plotly}, a
#' \code{\link[ggplot2]{ggplot}}/\code{\link[plotly]{plotly}} object, a data
#' frame with the source data, or a list containing both.
#'
#' @return
#' A \code{\link[ggplot2]{ggplot}} object.
#'
#'
#' @examples
#' \dontrun{
#' # Plot growth of $10k in VFINX and BRK-B
#' plot_growth(tickers = c("VFINX", "BRK-B"))
#' }
#'
#' @export
plot_growth <- function(prices = NULL,
                        tickers = NULL, ...,
                        gains = NULL,
                        initial = 10000,
                        plotly = FALSE,
                        title = "Growth Over Time",
                        base_size = 16,
                        tooltip_size = 20,
                        point_size = 1,
                        line_size = 1,
                        ticklabel_size = 8,
                        legend_position = "right",
                        return = "plot") {

  # Determine prices if not pre-specified
  if (is.null(prices)) {

    if (! is.null(gains)) {

      prices <- gains_prices(gains = gains, initial = initial)

    } else if (! is.null(tickers)) {

      prices <- load_prices(tickers = tickers, initial = initial, ...)

    } else {

      stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")

    }

  }

  # Transform for ggplot
  if (is.null(tickers)) tickers <- setdiff(names(prices), "Date")
  df <- prices %>%
    data.table::as.data.table() %>%
    data.table::melt(measure.vars = tickers,
                     variable.name = "Fund",
                     value.name = "Balance ($)") %>%
    as.data.frame()

  # Create plot
  df$tooltip <- paste(df$Fund,
                      "<br>", "Date: ", df$Date,
                      "<br>", "Balance: $", comma(df$`Balance ($)`, accuracy = 0.01), sep = "")
  p <- ggplot(df, aes(y = `Balance ($)`, x = Date, group = Fund, color = Fund, text = tooltip)) +
    geom_point(size = point_size) +
    geom_line(na.rm = TRUE, size = line_size) +
    scale_y_continuous(limits = range(c(0, df$Balance)) * 1.02, expand = c(0, 0), labels = comma) +
    theme_gray(base_size = base_size) +
    theme(axis.text = element_text(size = ticklabel_size)) +
    theme(legend.title = element_blank(), legend.position = legend_position) +
    labs(title = title)

  if (plotly) {
    p <- ggplotly(p, tooltip = "tooltip") %>%
      style(hoverlabel = list(font = list(size = tooltip_size)))
  }

  if (return == "plot") return(p)
  if (return == "data") return(df)
  return(list(plot = p, data = df))

}
