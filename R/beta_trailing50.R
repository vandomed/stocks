#' Calculate Beta Using Last 50 Daily Gains
#' 
#' Calculates beta for a ticker symbol based on the previous 50 daily gains.
#' 
#' 
#' @param ticker Character string with ticker symbol that Yahoo! Finance 
#' recognizes.
#' @param benchmark Character string specifying which fund to use as benchmark.
#' @param ... Arguments to pass to \code{\link{load_gains}}.
#' 
#' 
#' @return
#' Numeric value.
#' 
#' 
#' @references 
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial 
#' Modelling Framework. R package version 0.4-15. 
#' \url{https://CRAN.R-project.org/package=quantmod}
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
beta_trailing50 <- function(ticker, benchmark = "SPY", ...) {
  
  gains <- load_gains(tickers = c(benchmark, ticker), from = Sys.Date() - 90, ...)
  nrow_gains <- nrow(gains)
  gains <- gains[(nrow_gains - 49): nrow_gains, , drop = FALSE]
  as.numeric(lm(gains[[3]] ~ gains[[2]])$coef[2])
  
}
