#' Risk-Return Ratio
#' 
#' Calculates risk-return ratio, defined as growth rate divided by maximum 
#' drawdown.
#' 
#' 
#' @param gains Numeric vector of gains.
#' @param prices Numeric vector of prices.
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
#' # Convert to daily balances assuming an initial balance of $10,000
#' daily.balances <- gains_prices(stock.gains + 1)
#' 
#' # Total return is about 1.23
#' daily.balances[length(daily.balances)] / daily.balances[1] - 1
#' 
#' # Maximum drawdown is about 0.19
#' mdd(prices = daily.balances)
#' 
#' # Ratio of these two is about 6.48
#' (daily.balances[length(daily.balances)] / daily.balances[1] - 1) / 
#' mdd(daily.balances)
#' 
#' # Easier to calculate using rrr
#' rrr(daily.balances)
#' 
#' 
#' @export
rrr <- function(prices = NULL,
                gains = NULL) {
  
  if (! is.null(prices)) {
    return(prices_rate(prices) / mdd(prices = prices))
  }
  gains_rate(gains) / mdd(gains = gains)
  
}