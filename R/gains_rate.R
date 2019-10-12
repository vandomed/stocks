#' Calculate Growth Rate from Sequence of Gains
#' 
#' The formula is simply: \code{prod(gains + 1) - 1}. If \code{units.out} is 
#' specified, then it converts to x-unit growth rate.
#' 
#' 
#' @param gains Data frame with one column of gains for each investment (can be 
#' a numeric vector if there is only one).
#' @param units.out Numeric value specifying the number of units for growth 
#' rate calculation, if you want something other than total growth. For 
#' annualized growth rate, set to 252 if \code{gains} has daily gains, 12 if 
#' \code{gains} has monthly gains, etc.
#' 
#' 
#' @return Numeric vector.
#' 
#' 
#' @examples 
#' # Create vector of daily gains for a hypothetical stock
#' daily.gains <- c(-0.02, -0.01, 0.01, 0.02, 0.01)
#' 
#' # Overall growth is 0.95%
#' gains_rate(daily.gains)
#' 
#' # Average daily growth is 0.19%
#' gains_rate(daily.gains, 1)
#' 
#' # Corresponds to 61.0% annual growth
#' gains_rate(daily.gains, 252)
#' 
#' 
#' @export
gains_rate <- function(gains, units.out = NULL) {
  
  if (! any(class(gains) %in% c("numeric", "data.frame"))) {
    stop("The input 'gains' must be a numeric vector or data frame")
  }
  if (! is.null(units.out) && (units.out != as.integer(units.out) | units.out <= 0)) {
    stop("If specified, the input 'units.out' must be a positive integer")
  }
  
  if (is.vector(gains)) {
    
    units.in <- length(gains)
    rate1 <- prod(gains + 1) - 1
    
  } else {
    
    units.in <- nrow(gains)
    rate1 <- sapply(gains[sapply(gains, is.numeric)], function(x) prod(x + 1) - 1)
    
  }
  
  if (! is.null(units.out) && units.out != units.in) {
    rate1 <- convert_gain(gain = rate1, units.in = units.in, units.out = units.out)
  }
  
  return(rate1)
  
}
