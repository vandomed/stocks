#' Convert Prices to Gains
#' 
#' Calculates gains based on vector or matrix of prices.
#' 
#' 
#' @inheritParams metrics
#' 
#' 
#' @return Numeric vector or matrix.
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
  
  if (is.vector(prices)) {
    gains <- pchanges(prices)
  } else {
    gains <- apply(prices, 2, pchanges)
    rownames(gains) <- rownames(prices)[-1]
  }
  return(gains)
  
}
