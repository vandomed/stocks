#' Convert Vector of Gains to Prices
#' 
#' Calculates vector of prices based on initial balance and vector of gains. 
#' Defined simply as \code{initial * cumprod(gains + 1)}.
#' 
#' 
#' @param gains Numeric vector of gains.
#' @param initial Numeric value.
#' 
#' 
#' @return Numeric vector.
#' 
#' 
#' @examples 
#' # Simulate daily gains over a 5-year period
#' set.seed(123)
#' gains <- rnorm(n = 252 * 5, mean = 0.001, sd = 0.02)
#' 
#' # Plot balance over time if initial balance is $10,000
#' prices <- gains_prices(gains)
#' plot(prices)
#' 
#' 
#' @export
gains_prices <- function(gains, initial = 10000) {
  return(c(initial, initial * cumprod(1 + gains)))
}