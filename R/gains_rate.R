#' Calculate Growth Rate From a Vector of Gains
#' 
#' The formula is simply: \code{prod(gains + 1) - 1}. If \code{units.rate} is 
#' specified, then it converts to x-unit growth rate.
#' 
#' 
#' @param gains Numeric vector of gains.
#' @param units.rate Numeric value specifying the number of units for growth 
#' rate calculation, if you want something other than total growth. For 
#' annualized growth rate, set to 252 if \code{gains} has daily gains, 12 if 
#' \code{gains} has monthly gains, etc.
#' 
#' 
#' @return Numeric value.
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
gains_rate <- function(gains, units.rate = NULL) {
  
  # Calculate overall growth
  length.gains <- length(gains)
  rate1 <- prod(gains + 1) - 1
  
  # Convert to x-unit growth rate if xunit.rate is specified
  if (! is.null(units.rate) && ! (units.rate == length.gains)) {
    rate1 <- convert_gain(gain = rate1,
                          units.in = length.gains, units.out = units.rate)
  }
  
  # Return the rate
  return(rate1)
  
}