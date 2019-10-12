#' Calculate Growth Rate From a Vector of Prices
#' 
#' The formula is simply: \code{prices[length(prices)] / prices[1] - 1}. If 
#' \code{units.rate} is specified, then it converts to x-unit growth rate.
#' 
#' 
#' @param prices Numeric vector of prices or data frame with one column for each 
#' investment.
#' @param units.rate Numeric value specifying the number of units for growth 
#' rate calculation, if you want something other than total growth. For 
#' annualized growth rate, set to 252 if \code{prices} has daily prices, 12 if 
#' \code{prices} has monthly prices, etc.
#' 
#' 
#' @return Numeric value or vector.
#' 
#' 
#' @examples 
#' \dontrun{
#' # Load historical prices for SPY and TLT and then calculate growth rate
#' prices <- load_prices(tickers = c("SPY", "TLT"), mutual.start = TRUE)
#' prices_rate(prices)
#' # Plot mean vs. SD for UPRO/VBLTX/VWEHX
#' plot_metrics_3funds(mean ~ sd, tickers = c("UPRO", "VBLTX", "VWEHX"))
#' 
#' # Plot CAGR vs. MDD for FB/AAPL/NFLX and SPY/TLT/JNK
#' plot_metrics_3funds(cagr ~ mdd, tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK"))
#' 
#' # Plot Sharpe ratio vs. allocation for the same sets
#' plot_metrics_3funds(sharpe ~ allocation, tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK"))
#' }
#' 
#' # Create vector of daily closing prices for a hypothetical stock
#' prices <- c(100.4, 98.7, 101.3, 101.0, 100.9)
#' 
#' # Overall growth is 0.50%
#' prices_rate(prices)
#' 
#' # Average daily growth is 0.12%
#' prices_rate(prices, 1)
#' 
#' # Corresponds to 36.7% annualized growth
#' prices_rate(prices, 252)
#' 
#' 
#' @export
prices_rate <- function(prices, units.rate = NULL) {
  
  if (is.vector(prices)) {
    
    # Calculate overall growth
    length.prices <- length(prices)
    rate1 <- prices[length.prices] / prices[1] - 1
    
  } else {
    
    # Calculate overall growth for each fund
    length.prices <- nrow(prices)
    rate1 <- prices[length.prices, ] / prices[1, ] - 1
    
  }
  
  # Convert to x-unit growth rate if units.rate is specified
  if (! is.null(units.rate) && units.rate != length.prices - 1) {
    rate1 <- convert_gain(gain = rate1, 
                          units.in = length.prices - 1, 
                          units.out = units.rate)
  }
  
  # Return rate
  return(rate1)
  
}