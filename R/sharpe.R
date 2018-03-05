#' Sharpe Ratio
#' 
#' Calculates Sharpe ratio from vector of gains or prices. The formula is: 
#' \code{(mean(gains) - rf) / sd(gains)}, where \code{rf} is some risk-free rate 
#' of return.
#' 
#' 
#' @inheritParams metrics
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
    gains <- prices_gains(prices)
  }
    
  # Calculate and return Sharpe ratio
  if (is.vector(gains)) {
    sharpe.ratio <- (mean_n(gains) - rf) / sd_n(gains)
  } else {
    means <- apply(gains, 2, mean_n)
    sds <- apply(gains, 2, sd_n)
    sharpe.ratio <- (means - rf) / sds
  }
  return(sharpe.ratio)
  
}
