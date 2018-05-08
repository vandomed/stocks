#' Get Yahoo! Finance Start/End Dates for Tickers
#' 
#' Typically useful for determining a time period over which to compare several 
#' funds.
#' 
#' 
#' @param tickers Character vector with ticker symbols that Yahoo! Finance 
#' recognizes.
#' @param from Date or character string (e.g. \code{"2015-01-15"}.
#' @param to Date or character string (e.g. \code{"2016-01-30"}).
#' 
#' 
#' @return Data frame with ticker symbol, start date, end date, and number of 
#' trading days for each ticker.
#' 
#' 
#' @examples 
#' \dontrun{
#' # See what dates are available for Apple and Amazon
#' ticker_dates(c("AAPL", "AMZN"))
#' }
#' 
#' 
#' @references 
#' Ryan, J.A. and Ulrich, J.M. (2017) quantmod: Quantitative Financial Modelling 
#' Framework. R package version 0.4-12, 
#' \url{https://CRAN.R-project.org/package=quantmod}.
#' 
#' 
#' @export
ticker_dates <- function(tickers, from = "1950-01-01", to = Sys.Date()) {
  
  # Download prices from Yahoo! Finance using 'quantmod' package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices[[ii]] <- as.matrix(getSymbols(Symbols = tickers[ii],
                                         from = from, to = to,
                                         auto.assign = FALSE))
  }
  
  # Create data frame summarizing results
  ret <- data.frame(ticker = tickers,
                    start.date = as.Date(unlist(lapply(prices, function(x)
                      rownames(x)[1]))),
                    end.date = as.Date(unlist(lapply(prices, function(x)
                      rev(rownames(x))[1]))),
                    days = unlist(lapply(prices, function(x) nrow(x))),
                    stringsAsFactors = FALSE)
  return(ret)
  
}