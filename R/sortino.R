#' Sortino Ratio
#' 
#' Calculates Sortino ratio from vector of gains or prices. The formula is: 
#' \code{(mean(gains) - rf) / sd(gains[gains < 0])}, where \code{rf} is some 
#' risk-free rate of return.
#' 
#' 
#' @param gains Numeric vector of gains.
#' @param prices Numeric vector of prices.
#' @param rf Numeric value.
#' 
#' 
#' @return
#' Numeric value.
#' 
#' 
#' @examples
#' # Simulate daily gains over a 5-year period
#' set.seed(123)
#' stock.gains <- rnorm(252 * 5, 0.0005, 0.01)
#' 
#' # Calculate Sortino ratio using risk-free return of 0
#' sortino(stock.gains)
#' 
#' 
#' @export
sortino <- function(gains = NULL,
                    prices = NULL,
                    rf = 0) {
  
  if (! is.null(gains)) {
    return((mean(gains) - rf) / sd(gains[gains < 0]))
  }
  gains <- prices_gains(prices)
  (mean(gains) - rf) / sd(gains[gains < 0])
  
}