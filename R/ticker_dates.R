#' Get Yahoo! Finance Start/End Dates for Tickers
#' 
#' Useful for figuring out a time period over which to compare several funds.
#' 
#' 
#' @param tickers Character vector with ticker symbols that Yahoo! Finance 
#' recognizes.
#' @param from Date or character string (e.g. \code{"2015-01-15"}.
#' @param to Date or character string (e.g. \code{"2016-01-30"}).
#' 
#' 
#' @return Data frame with start and end dates for each fund.
#' 
#' 
#' @examples 
#' \dontrun{
#' # See what dates are available for AAPL and AMZN
#' ticker_dates(c("AAPL", "AMZN"))
#' }
#' 
#' 
#' @export
ticker_dates <- function(tickers, from = "1950-01-01", to = Sys.Date()) {
  
  # Download prices from Yahoo! Finance using quantmod::getSymbols
  dates <- sapply(tickers, function(x) {
    y <- as.matrix(getSymbols(Symbols = x, from = from, to = to, auto.assign = FALSE))
    rownames(y[c(1, nrow(y)), ])
  })
  as.data.frame(tibble(
    Ticker = tickers, 
    `Start date` = dates[1, ], 
    `End date` = dates[2, ]
  ))
  
}