#' Calculate Various Performance Metrics
#' 
#' Useful for comparing metrics for several investments. The first investment is 
#' used as the benchmark if requested metrics require one.
#' 
#' 
#' @param tickers Character vector of ticker symbols that Yahoo! Finance 
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param gains Numeric matrix with 1 column of gains for each investment (can 
#' be a vector if there is only one).
#' @param prices Numeric matrix with 1 column of prices for each investment (can 
#' be a vector if there is only one).
#' @param perf.metrics Character vector specifying metrics to calculate.
#' 
#' 
#' @return
#' List containing a data frame with the performance metrics and a correlation 
#' matrix for gains for the various investments.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' # # Calculate performance metrics for SSO and UPRO, using SPY as benchmark 
#' # # for alpha and beta
#' # metrics1 <- metrics(tickers = c("SPY", "SSO", "UPRO"))
#' 
#' 
#' @export
metrics <- function(tickers = NULL, ...,
                    gains = NULL,
                    prices = NULL,
                    perf.metrics = c("mean", "sd", "growth", "cagr", "mdd",
                                     "sharpe", "sortino", "alpha", "beta",
                                     "r.squared", "pearson", "spearman",
                                     "auto.pearson", "auto.spearman")) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Obtain matrix of gains for each fund
    gains <- load_gains(tickers = tickers, ...)
    
  } else if (! is.null(prices)) {
    
    # Convert to matrix if necessary
    if (! is.matrix(prices)) {
      prices <- matrix(prices, ncol = 1)
    }
    
    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges)
    rownames(gains) <- rownames(prices)[-1]
    
  } else if (! is.null(gains)) {
    
    # Convert to matrix if necessary
    if (! is.matrix(gains)) {
      gains <- matrix(gains, ncol = 1)
    }
    
  }
  
  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2,
  # ...
  tickers <- colnames(gains)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(gains))
  }
  
  # Figure out how many units are in a year, for CAGR and axis labels. If
  # unknown, assume daily.
  if (hasArg(time.scale)) {
    extra.args <- list(...)
    time.scale <- extra.args$time.scale
    units.year <- ifelse(time.scale == "daily", 252,
                         ifelse(time.scale == "monthly", 12,
                                1))
  } else {
    min.diffdates <-
      min(diff(as.Date(rownames(gains)[1: min(10, nrow(gains))])))
    if (! is.null(min.diffdates)) {
      if (min.diffdates == 1) {
        time.scale <- "daily"
        units.year <- 252
      } else if (min.diffdates >= 2 & min.diffdates <= 30) {
        time.scale <- "monthly"
        units.year <- 12
      } else if (min.diffdates > 30) {
        time.scale <- "yearly"
        units.year <- 1
      }
    } else {
      time.scale <- "daily"
      units.year <- 252
    }
  }
  
  # Calculate performance metrics for each fund
  p.metrics <- data.frame(ticker = tickers, stringsAsFactors = FALSE)
  if ("mean" %in% perf.metrics) {
    p.metrics$mean <- apply(gains, 2, mean)
  }
  if ("sd" %in% perf.metrics) {
    p.metrics$sd <- apply(gains, 2, sd)
  }
  if ("growth" %in% perf.metrics) {
    p.metrics$growth <- apply(gains, 2, function(x) gains_rate(gains = x))
  }
  if ("cagr" %in% perf.metrics) {
    p.metrics$cagr <- apply(gains, 2, function(x)
      gains_rate(gains = x, units.rate = units.year))
  }
  if ("mdd" %in% perf.metrics) {
    p.metrics$mdd <- apply(gains, 2, function(x) mdd(gains = x))
  }
  if ("sharpe" %in% perf.metrics) {
    p.metrics$sharpe <- apply(gains, 2, function(x) sharpe(gains = x))
  }
  if ("sortino" %in% perf.metrics) {
    p.metrics$sortino <- apply(gains, 2, function(x) sortino(gains = x))
  }
  if ("alpha" %in% perf.metrics) {
    p.metrics$alpha <- c(0, apply(gains[, -1, drop = F], 2, function(x)
      lm(x ~ gains[, 1])$coef[1]))
  }
  if ("beta" %in% perf.metrics) {
    p.metrics$beta <- c(1, apply(gains[, -1, drop = F], 2, function(x)
      lm(x ~ gains[, 1])$coef[2]))
  }
  if ("r.squared" %in% perf.metrics) {
    p.metrics$r.squared <- c(1, apply(gains[, -1, drop = F], 2, function(x)
      summary(lm(x ~ gains[, 1]))$r.squared))
  }
  if ("pearson" %in% perf.metrics) {
    p.metrics$pearson <- apply(gains, 2, function(x) cor(x, gains[, 1]))
  }
  if ("spearman" %in% perf.metrics) {
    p.metrics$spearman <- apply(gains, 2, function(x)
      cor(x, gains[, 1], method = "spearman"))
  }
  if ("auto.pearson" %in% perf.metrics) {
    p.metrics$auto.pearson <- apply(gains, 2, function(x)
      cor(x[-length(x)], x[-1]))
  }
  if ("auto.spearman" %in% perf.metrics) {
    p.metrics$auto.spearman <- apply(gains, 2, function(x)
      cor(x[-length(x)], x[-1], method = "spearman"))
  }
  
  # Calculate correlation matrix
  cor.mat <- cor(gains)
  
  # Return performance metrics and and correlation matrix
  return.list <- list(perf.metrics = p.metrics, cor.mat = cor.mat)
  return(return.list)
  
}