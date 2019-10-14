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
#' @param return Character string specifying what to return. Choices are 
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#' 
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
                     value.name = "Balance")
  
  # Create plot
  p <- ggplot(df, aes(x = Date, y = Balance, group = Fund, color = Fund)) + 
    geom_line(na.rm = TRUE) + 
    scale_y_continuous(labels = comma) + 
    theme(legend.title = element_blank()) +
    labs(title = "Balance over Time", y = "Balance ($)", x = "Date")
  
  return(p)
  
}
