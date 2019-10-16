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
#' @param ggplotly Logical value for whether to convert the 
#' \code{\link[ggplot2]{ggplot}} to a \code{\link[plotly]{ggplotly}} object 
#' internally.
#' @param return Character string specifying what to return. Choices are 
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#' 
#' 
#' @return
#' Depending on \code{return} and \code{ggplotly}, a 
#' \code{\link[ggplot2]{ggplot}}/\code{\link[plotly]{ggplotly}} object, a data 
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
                        ggplotly = FALSE, 
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
  tickers <- setdiff(names(prices), "Date")
  df <- prices %>% 
    data.table::as.data.table() %>% 
    data.table::melt(measure.vars = tickers, 
                     variable.name = "Fund", 
                     value.name = "Balance") %>%
    as.data.frame()
  
  # Create plot
  df$text <- paste("Fund: ", df$Fund,
                   "<br>", "Date: ", df$Date, 
                   "<br>", "Balance: $", comma(df$Balance, accuracy = 0.01), sep = "") 
  p <- ggplot(df, aes(y = Balance, x = Date, group = Fund, color = Fund, text = text)) + 
    geom_line(na.rm = TRUE) + 
    scale_y_continuous(labels = comma) + 
    theme(legend.title = element_blank()) +
    labs(title = "Balance over Time", y = "Balance ($)", x = "Date")
  
  if (ggplotly) p <- ggplotly(p, tooltip = "text")
  
  if (return == "plot") return(p)
  if (return == "data") return(df)
  if (return == "both") return(list(plot = p, data = df))
  return(p)
  
}
