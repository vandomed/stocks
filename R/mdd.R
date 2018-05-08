#' Maximum Drawdown
#' 
#' Calculates maximum drawdown from vector of closing prices, highs and lows, or 
#' gains. 
#' 
#' 
#' @inheritParams metrics
#' @param highs Numeric vector of daily high prices.
#' @param lows Numeric vector of daily low prices.
#' @param indices Logical value for whether to include indices for when the 
#' maximum drawdown occurred.
#' 
#' 
#' @return
#' Numeric value, vector, or matrix depending on \code{indices} and whether 
#' there is 1 fund or several.
#' 
#' 
#' @examples
#' \dontrun{
#' # Simulate minute-to-minute stock gains over a 2-year period
#' set.seed(123)
#' stock.gains <- rnorm(6.5 * 60 * 252 * 2, 0.000005, 0.001)
#' 
#' # Convert to stock prices assuming an initial price of $9.50 per share
#' stock.prices <- gains_prices(gains = stock.gains, initial = 9.50)
#' 
#' # Plot minute-to-minute stock prices (200k data point, may be slow)
#' plot(stock.prices)
#' 
#' # Maximum drawdown based on stock prices
#' mdd(prices = stock.prices)
#' 
#' # Same answer using gains rather than prices
#' mdd(gains = stock.gains)
#' }
#' 
#' 
#' @export
mdd <- function(prices = NULL, 
                highs = NULL, lows = NULL,
                gains = NULL,
                indices = FALSE) {
  
  # If gains specified rather than prices, convert to prices
  if (! is.null(gains)) {
    prices <- gains_prices(gains = gains, initial = 1)
  }
  
  if (! is.null(prices)) {
    
    if (is.vector(prices)) {
      
      # Vector of prices input
      if (indices) {
        mdd.out <- .Call(`_stocks_mdd_p_indices`, prices)
        names(mdd.out) <- c("mdd", "start.index", "end.index")
      } else {
        mdd.out <- .Call(`_stocks_mdd_p`, prices)
      }
      
    } else {
      
      # Matrix of prices input
      mdd.out <- apply(prices, 2, function(x) 
        mdd(prices = x, indices = indices))
      
    }
    
  } else {
    
    # Vector of highs and lows input
    if (indices) {
      mdd.out <- .Call(`_stocks_mdd_hl_indices`, highs, lows)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
    } else {
      mdd.out <- .Call(`_stocks_mdd_hl`, highs, lows)
    }
    
  }
  
  # Return mdd.out
  return(mdd.out)
  
}
