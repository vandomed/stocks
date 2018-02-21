#' Sharpe Ratio
#' 
#' Calculates Sharpe ratio from vector of gains or prices. The formula is: 
#' \code{(mean(gains) - rf) / sd(gains)}, where \code{rf} is some risk-free rate 
#' of return.
#' 
#' 
#' @param gains Numeric vector.
#' @param prices Numeric vector.
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
#' # Calculate Sharpe ratio using risk-free return of 0
#' sharpe(stock.gains)
#' 
#' 
#' @export
sharpe <- function(gains = NULL,
                   prices = NULL,
                   rf = 0) {
  
  # Convert from prices to gains if necessary
  if (! is.null(prices)) {
    gains <- pchanges(prices)
  }
    
  # Calculate and return Sharpe ratio
  return((mean(gains) - rf) / sd(gains))
  
}