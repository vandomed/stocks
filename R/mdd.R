#' Maximum Drawdown
#' 
#' Calculates maximum drawdown from vector of closing prices, highs and lows, or 
#' gains. Missing values should be removed prior to calling this function.
#' 
#' 
#' @param prices 
#' @param highs Numeric vector of daily high prices.
#' @param lows Numeric vector of daily low prices.
#' @param gains Data frame with one column of gains for each investment (extra 
#' non-numeric columns are ignored), or numeric vector for one investment.
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
#' # Calculate MDD's for FANG stocks in 2018
#' prices <- load_prices(c("FB", "AAPL", "NFLX", "GOOG"), from = "2018-01-01", to = "2018-12-31")
#' sapply(prices[-1], mdd)
#' }
#' 
#' 
#' @export
mdd <- function(prices = NULL, 
                highs = NULL, lows = NULL,
                gains = NULL,
                indices = FALSE) {
  
  if (! is.null(gains)) {
    prices <- gains_prices(gains = gains, initial = 1)
  }
  
  if (! is.null(prices)) {
    
    if (indices) {
      mdd.out <- .Call(`_stocks_mdd_p_indices`, prices)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
      return(mdd.out)
    }
    return(.Call(`_stocks_mdd_p`, prices))
    
  }
  
  if (indices) {
    mdd.out <- .Call(`_stocks_mdd_hl_indices`, highs, lows)
    names(mdd.out) <- c("mdd", "start.index", "end.index")
    return(mdd.out)
  } 
  
  return(.Call(`_stocks_mdd_hl`, highs, lows))
  
}
