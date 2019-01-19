#' Convert Gains to Prices
#' 
#' Calculates prices based on initial balance and vector of gains.
#' 
#' 
#' @inheritParams metrics
#' @param initial Numeric value.
#' 
#' 
#' @return Numeric value if \code{gains} is a vector, numeric matrix if 
#' \code{gains} is a matrix.
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
  if (is.vector(gains)) {
    prices <- c(initial, initial * cumprod(1 + gains))
    date1 <- names(gains[1])
    if (! is.null(date1)) {
      names(prices)[1] <- as.character(as.Date(date1) - 1)
    }
  } else {
    prices <- apply(gains, 2, function(x) {
      c(initial, initial * cumprod(1 + x))
    })
    date1 <- rownames(gains)[1]
    if (! is.null(date1)) {
      rownames(prices)[1] <- as.character(as.Date(date1) - 1)
    }
  }
  return(prices)
}