#' Convert Sequence of Gains to Sequence of Prices
#' 
#' Converts sequence of gains and initial balance to sequence of prices for one 
#' or more investments. 
#' 
#' 
#' @param gains Numeric vector of gains for one investment, or data frame with 
#' one column for each investment and an optional Date variable.
#' @param initial Numeric value.
#' @param date1 Date to use for initial price.
#' 
#' 
#' @return Numeric vector or data frame.
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
gains_prices <- function(gains, initial = 10000, date1 = NULL) {
  
  if (! any(class(gains) %in% c("numeric", "data.frame"))) {
    stop("The input 'gains' must be a numeric vector or data frame")
  }
  
  if (is.vector(gains)) {
    if (is.na(gains[1])) {
      lastNA <- which.max(! is.na(gains)) - 1
      prices <- c(rep(NA, lastNA), initial, initial * cumprod(1 + gains[-c(1: lastNA)]))
      names(prices)[1] <- date1
      return(prices)
    }
    prices <- c(initial, initial * cumprod(1 + gains))
    names(prices)[1] <- date1
    return(prices)
  }
  
  prices <- as.data.frame(lapply(gains, function(x) {
    if (is.numeric(x)) {
      if (is.na(x[1])) {
        lastNA <- which.max(! is.na(x)) - 1
        return(c(rep(NA, lastNA), initial, initial * cumprod(1 + x[-c(1: lastNA)])))
      }
      return(c(initial, initial * cumprod(1 + x)))
    } else {
      if (is.null(date1)) return(c(x[1] - 1, x))
      return(c(date1, x))
    }
  }))
  names(prices) <- names(gains)
  return(prices)
  
}
