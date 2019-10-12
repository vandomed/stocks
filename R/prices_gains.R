#' Convert Sequence of Prices to Sequence of Gains
#' 
#' Converts sequence of prices to sequence of gains for one or more investments.
#' 
#' 
#' @param prices Numeric vector of prices for one investment or data frame with 
#' one column for each investment and an optional Date variable.
#' 
#' 
#' @return Numeric vector or data frame.
#' 
#' 
#' @examples
#' \dontrun{
#' # Load 2017 prices for Netflix and Amazon, and calculate growth of $10k
#' prices <- load_prices(c("NFLX", "AMZN"), initial = 1000)
#' 
#' # Calculate gains
#' gains <- prices_gains(prices)
#' }
#' 
#' 
#' @export
prices_gains <- function(prices) {
  
  if (! any(class(prices) %in% c("numeric", "data.frame"))) {
    stop("The input 'gains' must be a numeric vector or data frame")
  }
  
  if (is.vector(prices)) {
    return(pchanges(prices))
  }
  numeric.vars <- sapply(prices, is.numeric)
  gains <- cbind(prices[! numeric.vars][-1, , drop = FALSE], 
                 sapply(prices[numeric.vars], pchanges))
  names(gains) <- c(names(prices)[! numeric.vars], names(prices)[numeric.vars])
  return(gains)
  
}