#' Backtest a Fixed-Allocation Trading Strategy
#' 
#' Implements a trading strategy aimed at maintaining a fixed allocation to each 
#' of several funds, rebalancing when the effective allocations deviate too far 
#' from the targets.
#' 
#' 
#' @inheritParams twofunds_graph
#' @inheritParams load_prices
#' 
#' @param target.alls Numeric vector specifying target allocations to each fund. 
#' If unspecified, equal allocations are used (e.g. 1/3, 1/3, 1/3 if there are 
#' 3 funds).
#' 
#' @param tol Numeric value indicating how far the effective allocations can 
#' drift away from the targets before rebalancing.
#' 
#' @param rebalance.cost Numeric value specifying total cost of each rebalancing 
#' trade.
#'
#' 
#' @return
#' List containing:
#' \enumerate{
#' \item Numeric matrix named \code{fund.balances} giving fund balances over 
#' time.
#' \item Numeric value named \code{rebalance.count} giving the number of 
#' rebalancing trades executed.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Backtest equal-allocation UPRO/VBLTX/VWEHX strategy
#' port <- targetall(tickers = c("UPRO", "VBLTX", "VWEHX"))
#' plot(port$fund.balances[, "Portfolio"])
#' }
#'
#' @export
targetall <- function(tickers = NULL, intercepts = NULL, slopes = NULL, ...,
                      tickers.gains = NULL,
                      target.alls = NULL,
                      tol = 0.05,
                      rebalance.cost = 0,
                      initial = 10000) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Get number of tickers
    n.tickers <- length(tickers)
    
    # If unspecified, use equal target allocations
    if (is.null(target.alls)) {
      target.alls <- rep(1 / n.tickers, n.tickers)
    }
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- rep(0, n.tickers)
    }
    if (is.null(slopes)) {
      slopes <- rep(1, n.tickers)
    }
    
    # Calculate tickers.gains matrix
    tickers.gains <- load_gains(tickers = tickers, intercepts = intercepts,
                                slopes = slopes, ...)
    
    # Update ticker names to show intercept/slope
    tickers <- colnames(tickers.gains)[1: n.tickers]
    
  } else {
    
    # Get number of tickers
    n.tickers <- ncol(tickers.gains)
    
    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: n.tickers, sep = "")
    }
    
  }
  
  # Create tickers.ratios matrix
  tickers.ratios <- tickers.gains + 1
  
  # Initialize variables used in looping through daily gains
  fund.balances <- matrix(NA, ncol = n.tickers + 1, 
                          nrow = nrow(tickers.gains) + 1)
  rebalance.count <- 0
  
  # Calculate initial balances in each fund
  port.bal <- initial
  funds.bal <- port.bal * target.alls
  fund.balances[1, ] <- c(funds.bal, port.bal)
  
  # Loop through daily gains and update portfolio and fund balances
  for (ii in 1: nrow(tickers.gains)) {
    
    # Update balances
    funds.bal <- funds.bal * tickers.ratios[ii, ]
    port.bal <- sum(funds.bal)
    fund.balances[(ii + 1), ] <- c(funds.bal, port.bal)
    
    # Check fund allocations
    funds.all <- funds.bal / port.bal
    
    # Rebalance if necessary
    if (any(abs(funds.all - target.alls) > tol) & ii < nrow(tickers.gains)) {
      rebalance.count <- rebalance.count + 1
      port.bal <- port.bal - rebalance.cost
      funds.bal <- port.bal * target.alls
    }
    
  }
  
  # Add column names to fund.balances
  colnames(fund.balances) <- c(tickers, "Portfolio")
  
  # Return list containing fund.balances and rebalance.count
  results.list <- list(fund.balances = fund.balances,
                       rebalance.count = rebalance.count)
  return(results.list)
  
}
