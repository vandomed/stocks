#' Maximum Drawdown
#' 
#' Calculates maximum drawdown from vector of closing prices, highs and lows, or 
#' gains.
#' 
#' 
#' @param prices Numeric vector. Can be daily closing prices, but intraday 
#' prices are preferred since maximum drawdown is really calculated from overall 
#' highs and lows, not closing prices.
#' @param highs Numeric vector of daily high prices.
#' @param lows Numeric vector of daily low prices.
#' @param gains Numeric vector.
#' @param indices Logical value for whether to include indices for when the 
#' maximum drawdown occurred.
#' 
#' 
#' @return
#' if \code{indices = FALSE}, numeric value with maximum drawdown; if 
#' \code{indices = TRUE}, numeric vector with maximum drawdown and 
#' indices for start/end of the drawdown period.
#' 
#' 
#' @examples
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
#' 
#' 
#' @export
mdd <- function(prices = NULL, 
                highs = NULL, lows = NULL,
                gains = NULL,
                indices = FALSE) {
  
  # If gains specified rather than prices, convert to prices
  if (!is.null(gains)) {
    prices <- gains_prices(gains = gains, initial = 1)
  }
  
  # Call C++ function depending on indices and whether prices or highs and lows
  # specified
  if (! is.null(prices)) {
    if (indices) {
      mdd.out <- .Call(`_stocks_mdd_p_indices`, prices)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
    } else {
      mdd.out <- .Call(`_stocks_mdd_p`, prices)
    }
  } else {
    if (indices) {
      mdd.out <- .Call(`_stocks_mdd_hl_indices`, prices)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
    } else {
      mdd.out <- .Call(`_stocks_mdd_hl`, prices)
    }
  }
  
  # Return mdd.out
  return(mdd.out)
  
}