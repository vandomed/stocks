#' Beta for Last 50 Daily Gains
#' 
#' Calculates beta for a ticker symbol based on the previous 50 daily gains.
#' 
#' 
#' @param ticker Character string with ticker symbols that Yahoo! Finance 
#' recognizes.
#' @param bench Character string with ticker symbol for benchmark.
#' @param ... Arguments to pass to \code{\link{load_gains}}.
#' 
#' 
#' @return
#' Numeric value.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Calculate TLT's beta based on the previous 50 daily gains
#' beta_trailing50("TLT")
#' }
#' 
#' 
#' @export
beta_trailing50 <- function(ticker, bench = "SPY", ...) {
  
  # Load gains for last 90 calendar days
  gains <- load_gains(tickers = c(bench, ticker),
                      from = Sys.Date() - 90, ...)
  
  # Get subset of most recent 50 gains
  gains.last50 <- gains[(nrow(gains) - 49): nrow(gains), ]
  
  # Calculate and return beta
  ticker.beta <- as.numeric(lm(gains.last50[, 2] ~ gains.last50[, 1])$coef[2])
  return(ticker.beta)
  
}