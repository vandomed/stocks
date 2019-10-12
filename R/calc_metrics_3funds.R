#' Calculate Performance Metrics for Three-Fund Portfolios with Varying 
#' Allocations
#' 
#' Useful for assessing the characteristics of three-fund portfolios.
#' 
#' 
#' @param gains Data frame with a date variable named Date and one column of 
#' gains for each investment.
#' @param formula Formula specifying what to plot, e.g. \code{mean ~ sd}, 
#' \code{cagr ~ mdd}, or \code{sharpe ~ allocation}. See \code{?calc_metrics} 
#' for list of metrics to choose from (\code{"allocation"} is an extra option 
#' here).
#' @param tickers Character vector of ticker symbols, where the first three are 
#' are a three-fund set, the next three are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param prices Data frame with a date variable named Date and one column of 
#' prices for each investment.
#' @param benchmark Character string specifying which fund to use as a 
#' benchmark for metrics that require one.
#' 
#' 
#' @return
#' Depending on \code{return}, a \code{\link[ggplot2]{ggplot}} object, a data
#' frame, or a list containing both.
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot mean vs. SD for UPRO/VBLTX/VWEHX
#' plot_metrics_3funds(mean ~ sd, tickers = c("UPRO", "VBLTX", "VWEHX"))
#' 
#' # Plot CAGR vs. MDD for FB/AAPL/NFLX and SPY/TLT/JNK
#' plot_metrics_3funds(cagr ~ mdd, tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK"))
#' 
#' # Plot Sharpe ratio vs. allocation for the same sets
#' plot_metrics_3funds(sharpe ~ allocation, tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK"))
#' }
#' 
#'
#' @export
calc_metrics_3funds <- function(gains = NULL, 
                                metrics = c("mean", "sd"), 
                                tickers = NULL, ..., 
                                step1 = 5, 
                                step2 = step1,  
                                prices = NULL, 
                                benchmark = "SPY") {
  
  # Check that requested metrics are valid
  invalid.requests <- setdiff(metrics, names(metric.info$label))
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):", 
               paste(invalid.requests, collapse = ", ")))
  }
  
  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% metrics)) {
    benchmark <- NULL
  }
  
  # Drop reference tickers that also appear in tickers
  ref.tickers <- setdiff(ref.tickers, tickers)
  if (length(ref.tickers) == 0) ref.tickers <- NULL
  
  # Determine gains if not pre-specified
  if (is.null(gains)) {
    
    if (! is.null(prices)) {
      
      date.var <- names(prices) == "Date"
      gains <- cbind(prices[-1, date.var, drop = FALSE], 
                     sapply(prices[! date.var], pchanges))
      
    } else if (! is.null(tickers)) {
      
      gains <- load_gains(tickers = unique(c(benchmark, tickers)), 
                          mutual.start = TRUE, mutual.end = TRUE, ...)
      
    } else {
      
      stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")
      
    }
    
  }
  
  # If tickers is NULL, set to all funds in gains
  if (is.null(tickers)) tickers <- setdiff(names(gains), "Date")
  
  # Drop NA's
  gains <- gains[complete.cases(gains), , drop = FALSE]
  
  # Figure out conversion factor in case CAGR or annualized alpha is requested
  min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
  units.year <- ifelse(min.diffdates == 1, 252, ifelse(min.diffdates <= 30, 12, 1))
  
  if (! is.null(benchmark)) {
    benchmark.gains <- gains[[benchmark]]
  } else {
    benchmark.gains <- NULL
    metrics <- setdiff(metrics, c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman"))
  }
  
  # Calculate metrics for each trio
  weights <- sapply(seq(0, 1, step1 / 100), function(x) {
    c2 <- seq(0, 1 - x, step2 / 100)
    rbind(x, c2, 1 - x - c2)
  })
  weights <- do.call(cbind, weights)
  w1 <- weights[1, ] * 100
  w2 <- weights[2, ] * 100
  w3 <- weights[3, ] * 100
  
  df <- lapply(seq(1, length(tickers), 3), function(x) {
    gains.trio <- as.matrix(gains[tickers[x: (x + 2)]]) 
    wgains.trio <- gains.trio %*% weights
    df.trio <- tibble(
      Trio = paste(colnames(gains.trio), collapse = "-"), 
      `Fund 1` = colnames(gains.trio)[1], 
      `Fund 2` = colnames(gains.trio)[2], 
      `Fund 3` = colnames(gains.trio)[3], 
      `Allocation 1 (%)` = w1, 
      `Allocation 2 (%)` = w2, 
      `Allocation 3 (%)` = w3, 
      `Allocation (%)` = `Allocation 1 (%)`
    )
    for (x in metrics) {
      df.trio[[metric.info$label[x]]] <- apply(wgains.trio, 2, function(x) {
        calc_metric(gains = x, metric = x, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    return(df.trio)
  })
  as.data.frame(bind_rows(df))
  
}