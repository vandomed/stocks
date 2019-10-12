#' Download Historical Gains
#' 
#' Downloads historical gains for specified tickers from Yahoo! Finance, with 
#' various options. Relies heavily on the \pkg{quantmod} package.
#' 
#' 
#' @param tickers Character vector of ticker symbols that Yahoo! Finance 
#' recognizes.
#' @param intercepts Numeric vector of values to add to daily gains for each 
#' fund.
#' @param slopes Numeric vector of values to multiply daily gains for each fund 
#' by. Slopes are multiplied prior to adding intercepts.
#' @param from Date or character string, e.g. \code{"2015-01-15"}.
#' @param to Date or character string, e.g. \code{"2018-12-31"}.
#' @param time.scale Character string. Choices are \code{"daily"}, 
#' \code{"monthly"}, and \code{"yearly"}.
#' @param preto.days Numeric value. If specified, function returns gains for 
#' \code{preto.days} trading days prior to \code{to}. For example, to load the 
#' most recent 50 daily gains, leave \code{to} and \code{time.scale} as the 
#' defaults and set \code{preto.days = 50}.
#' @param prefrom.days Numeric value. If specified, function returns gains for 
#' \code{prefrom.days} trading days prior to \code{from}. Useful when you want 
#' to test a trading strategy starting on a particular date, but the strategy 
#' requires data leading up to that date (e.g. trailing beta).
#' @param mutual.start Logical value for whether to start on the first day of  
#' the funds' mutual lifetimes.
#' @param mutual.end Logical value for whether to end on the last day of the 
#' funds' mutual lifetimes. 
#' @param drop.anyNA Logical value for whether to drop dates on which prices are
#' missing for any of the funds.
#' 
#' 
#' @return Data frame with gains for each fund.
#' 
#' 
#' @examples 
#' \dontrun{
#' # Load gains for Netflix and Amazon over their mutual lifetimes
#' gains <- load_gains(c("NFLX", "AMZN"))
#' }
#' 
#' 
#' @references 
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial 
#' Modelling Framework. R package version 0.4-15. 
#' \url{https://CRAN.R-project.org/package=quantmod}
#' 
#' 
#' @export
load_gains <- function(tickers, 
                       intercepts = NULL, 
                       slopes = NULL, 
                       from = "1950-01-01", 
                       to = Sys.Date(),
                       time.scale = "daily",
                       preto.days = NULL, 
                       prefrom.days = NULL,
                       mutual.start = FALSE, 
                       mutual.end = TRUE, 
                       drop.anyNA = FALSE) {
  
  prices <- load_prices(
    tickers = tickers, 
    intercepts = intercepts, 
    slopes = slopes, 
    from = from, 
    to = to, 
    time.scale = time.scale, 
    preto.days = preto.days, 
    prefrom.days = prefrom.days, 
    mutual.start = mutual.start, 
    mutual.end = mutual.end, 
    drop.anyNA = drop.anyNA
  )
  cbind(Date = prices[-1, 1, drop = FALSE], sapply(prices[, -1, drop = FALSE], pchanges))
  
}
