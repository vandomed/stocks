#' Sortino Ratio
#' 
#' Calculates Sortino ratio from vector of gains or prices. The formula is: 
#' \code{(mean(gains) - rf) / sd(gains[gains < 0])}, where \code{rf} is some 
#' risk-free rate of return.
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
#' # Calculate Sortino ratio using risk-free return of 0
#' sortino.ratio(stock.gains)
#' 
#' 
#' @export
sortino <- function(gains = NULL,
                    prices = NULL,
                    rf = 0) {
  
  # Convert from prices to gains if necessary
  if (!is.null(prices)) {
    gains <- gains[!is.na(gains)]
  }
  
  # Calculate and return Sortino ratio
  return((mean(gains) - rf) / sd(gains[gains < 0]))
  
}