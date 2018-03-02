#' Backtest a Simple Contango-Based Volatility Trading Strategy
#' 
#' Simple strategy: Each day, hold XIV if contango > \code{xiv.cutpoint}, hold 
#' VXX if contango < \code{vxx.cutpoint}, and hold cash otherwise. Perhaps not 
#' very useful since XIV closed on Feb. 20, 2018.
#' 
#' You can find historical contango values from The Intelligent Investor Blog. 
#' You can click the first link at \url{http://investing.kuchita.com/2012/06/28/xiv-data-and-pricing-model-since-vix-futures-available-2004/} to download a zip file containing an Excel spreadsheet. Then, 
#' you will need to calculate whatever version of "contango" you prefer. I 
#' typically define contango as what percent higher the second-month VIX futures 
#' are acompared to the first-month futures, i.e. dividing the "2nd mth" column 
#' by the "1st mth" column, subtracting 1, and then multiplying by 100.
#' 
#' I think the most common approach for contango-based volatility strategies is 
#' holding XIV (inverse volatility) when contango is above some value (e.g. 0\%, 
#' 5\%, or 10\%), and holding cash otherwise. You can do that with this function 
#' by leaving \code{vxx.cutpoint} as \code{-Inf}. However, you may also want to 
#' hold VXX (volatility) when contango is below some value 
#' (e.g. 0\%, -5\%, -10\%), also known as "backwardation". You can implement an 
#' XIV-only, VXX-only, or XIV and VXX strategy with this function.
#' 
#' To load daily gains for XIV and/or VXX, you can use \code{\link{load_gains}}, 
#' which uses the \pkg{quantmod} package [1] to load data from Yahoo! Finance. 
#' You will have to specify the \code{from} and \code{to} inputs to match the 
#' date range for your contango values.
#' 
#' @param contango Numeric vector of contango values at the end of each trading 
#' day.
#' 
#' @param xiv.gains Numeric vector of gains for XIV. Should be same length as 
#' \code{contango} and date-shifted one value to the right. For example, the 
#' first value of \code{xiv.gains} should be the XIV gain for the day AFTER the 
#' first contango value.
#' 
#' @param vxx.gains Numeric vector of gains for VXX. Should be same length as 
#' \code{contango} and date-shifted one value to the right. For example, the 
#' first value of \code{vxx.gains} should be the VXX gain for the day AFTER the 
#' first contango value.
#' 
#' @param xiv.cutpoint Numeric value giving the contango cutpoint for XIV, in 
#' percent. 
#' 
#' @param vxx.cutpoint Numeric value giving the contango cutpoint for VXX, in 
#' percent.
#' 
#' @param initial Numeric value giving the initial value of the portfolio.
#'
#' 
#' @return
#' List containing: 
#' \enumerate{
#' \item Character vector named \code{holdings} indicating what fund was held 
#' each day (XIV, VXX, or cash). 
#' \item Numeric vector named \code{port.gains} giving the portfolio gain for 
#' each day, which will be 0 for days that cash was held and the XIV or VXX gain 
#' for days that XIV or VXX was held. 
#' \item Numeric vector named \code{port.balances} giving the portfolio balance 
#' each day. 
#' \item Numeric value named \code{trades} giving the total number of trades 
#' executed.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#'
#' @export
contango_simple <- function(contango,
                            xiv.gains = NULL,
                            vxx.gains = NULL,
                            xiv.cutpoint = 0,
                            vxx.cutpoint = -Inf,
                            initial = 10000) {
  
  # Initialize data frame to record holding, gain, and portfolio balance for
  # each day
  results <- data.frame()
  
  # Create vector of fund held each day, and vector of portfolio gain for each
  # day
  holdings <- rep("cash", length(contango))
  port.gains <- rep(0, length(contango))
  locs.xiv <- which(contango > xiv.cutpoint)
  if (length(locs.xiv) > 0) {
    holdings[locs.xiv] <- "XIV"
    port.gains[locs.xiv] <- xiv.gains[locs.xiv]
  }
  locs.vxx <- which(contango < vxx.cutpoint)
  if (length(locs.vxx) > 0) {
    holdings[locs.vxx] <- "VXX"
    port.gains[locs.vxx] <- vxx.gains[locs.vxx]
  }
  
  # Calculate portfolio balance over time
  port.balances <- gains_prices(gains = port.gains, initial = initial)
  
  # Calculate number of trades
  trades <- length(rle(holdings)$lengths)
  
  # Compile results into list and return it
  results.list <- list(holdings = holdings,
                       port.gains = port.gains,
                       port.balances = port.balances,
                       trades = trades)
  return(results.list)
  
}
