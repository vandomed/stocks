#' Backtest a Hedged Contango-Based Volatility Trading Strategy
#' 
#' Implements the following strategy: Each day, hold XIV/SPXU (weighted for zero 
#' beta) if contango > \code{xiv.spxu.cutpoint}, hold VXX/UPRO (weighted for 
#' zero beta) if contango < \code{vxx.upro.cutpoint}, and hold cash otherwise. 
#' Perhaps not very useful since XIV closed on Feb. 20, 2018.
#' 
#' You can find historical contango values from The Intelligent Investor Blog. 
#' You can click the first link at 
#' \url{http://investing.kuchita.com/2012/06/28/xiv-data-and-pricing-model-since-vix-futures-available-2004/} 
#' to download a zip file containing an Excel spreadsheet. Then, you will need 
#' to calculate whatever version of "contango" you prefer. I typically define 
#' contango as what percent higher the second-month VIX futures are acompared to 
#' the first-month futures, i.e. dividing the "2nd mth" column by the "1st mth" 
#' column, subtracting 1, and then multiplying by 100.
#' 
#' To load daily gains for XIV, SPXU, VXX, and UPRO, you can use 
#' \code{\link{load_gains}}, which uses the \pkg{quantmod} package to load 
#' data from Yahoo! Finance. You will have to specify the \code{from} and 
#' \code{to} inputs to match the date range for your contango values.
#' 
#' 
#' @inheritParams contango_simple
#' 
#' @param xiv.spxu.gains 2-column numeric matrix with gains for XIV and SPXU. 
#' Should have the same number of rows as \code{contango} and be date-shifted 
#' one value to the right. For example, the first row should have the XIV and 
#' SPXU gains for the day AFTER the first contango value.
#' 
#' @param vxx.upro.gains 2-column numeric matrix with gains for VXX and UPRO. 
#' Should have the same number of rows as \code{contango} and be date-shifted 
#' one value to the right. For example, the first row should have the VXX and 
#' UPRO gains for the day AFTER the first contango value.
#' 
#' @param xiv.spxu.cutpoint Numeric value giving the contango cutpoint for  
#' XIV/SPXU position. For example, if \code{xiv.spxu.cutpoint = 5}, XIV/SPXU 
#' will be held whenever contango is greater than 5\%.
#' 
#' @param vxx.upro.cutpoint Numeric value giving the contango cutpoint for 
#' VXX/UPRO position. For example, if \code{vxx.upro.cutpoint = -5}, VXX/UPRO 
#' will be held whenever contango is less than -5\%.
#' 
#' @param xiv.allocation Numeric value specifying XIV allocation for XIV/SPXU 
#' position. For example, if set to 0.46, 46\% is allocated to XIV and 54\% to 
#' SPXU when contango > \code{xiv.spxu.cutpoint}.
#' 
#' @param vxx.allocation Numeric value specifying VXX allocation for VXX/UPRO 
#' position. For example, if set to 0.46, 46\% is allocated to VXX and 54\% to 
#' UPRO when contango < \code{vxx.upro.cutpoint}.
#' 
#' @param xiv.beta Numeric value specifying XIV's beta. If specified, the 
#' function figures out what \code{xiv.allocation} needs to be for zero-beta 
#' XIV/SPXU positions. For example, if set to 3.5, then 46.2\% XIV/53.8\% SPXU 
#' achieves zero beta.
#' 
#' @param vxx.beta Numeric value indicating VXX's beta. If specified, the 
#' function figures out what \code{vxx.allocation} needs to be for zero-beta 
#' VXX/UPRO positions. For example, if set to -3.5, then 46.2\% VXX/53.8\% UPRO 
#' achieves zero beta.
#' 
#' 
#' @return
#' List containing:
#' \enumerate{
#' \item Character vector named \code{holdings} indicating what fund was held 
#' each day (XIV/SPXU, VXX/UPRO, or cash). 
#' \item Numeric vector named \code{port.gains} giving the portfolio gain for 
#' each day, which will be 0 for days that cash was held and the weighted 
#' XIV/SPXU or VXX/UPRO gain for days that one of those positions was held. 
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
contango_hedged <- function(contango,
                            xiv.spxu.gains = NULL, vxx.upro.gains = NULL,
                            xiv.spxu.cutpoint = 6.36, vxx.upro.cutpoint = 5.45,
                            xiv.allocation = 0.46, vxx.allocation = 0.46,
                            xiv.beta = NULL, vxx.beta = NULL,
                            initial = 10000) {
  
  # If betas specified, calculate allocations for zero-beta XIV/SPXU and
  # VXX/UPRO
  if (! is.null(xiv.beta)) {
    xiv.allocation <- 3 / (xiv.beta + 3)
  }
  if (! is.null(vxx.beta)) {
    vxx.allocation <- -3 / (vxx.beta - 3)
  }
  
  # Calculate weighted XIV/SPXU gains and weighted VXX/UPRO gains
  if (! is.null(xiv.spxu.gains)) {
    xiv.spxu.weighted <- xiv.spxu.gains %*% c(xiv.allocation, 1 -
                                                xiv.allocation)
  }
  if (! is.null(vxx.upro.gains)) {
    vxx.upro.weighted <- vxx.upro.gains %*% c(vxx.allocation, 1 -
                                                vxx.allocation)
  }
  
  # Initialize data frame to record holding, gain, and portfolio balance for
  # each day
  results <- data.frame()
  
  # Create vector of fund held each day, and vector of portfolio gain for each
  # day
  holdings <- rep("Cash", length(contango))
  port.gains <- rep(0, length(contango))
  locs.xiv.spxu <- which(contango > xiv.spxu.cutpoint)
  if (length(locs.xiv.spxu) > 0) {
    holdings[locs.xiv.spxu] <- "XIV.SPXU"
    port.gains[locs.xiv.spxu] <- xiv.spxu.weighted[locs.xiv.spxu]
  }
  locs.vxx.upro <- which(contango < vxx.upro.cutpoint)
  if (length(locs.vxx.upro) > 0) {
    holdings[locs.vxx.upro] <- "VXX.UPRO"
    port.gains[locs.vxx.upro] <- vxx.upro.weighted[locs.vxx.upro]
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
