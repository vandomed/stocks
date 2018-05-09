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
#' List containing:
#' \enumerate{
#' \item Numeric matrix named \code{perf.metrics} with performance metrics. 
#' \item Numeric matrix named \code{cor.mat} with correlation matrix for gains 
#' for the various investments.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Calculate performance metrics for SSO and UPRO, using SPY as benchmark 
#' # for alpha and beta
#' metrics1 <- metrics(tickers = c("SPY", "SSO", "UPRO"))
#' }
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
  p.metrics <- NULL
  labels <- c()
  for (ii in 1: length(perf.metrics)) {
    
    metric.ii <- perf.metrics[ii]
    if (metric.ii == "mean") {
      p.metrics <- cbind(p.metrics, apply(gains, 2, mean))
      labels[ii] <- "Mean"
    } else if (metric.ii == "sd") {
      p.metrics <- cbind(p.metrics, apply(gains, 2, sd))
      labels[ii] <- "SD"
    } else if (metric.ii == "growth") {
      p.metrics <- cbind(p.metrics, gains_rate(gains))
      labels[ii] <- "Growth"
    } else if (metric.ii == "cagr") {
      p.metrics <- cbind(p.metrics, gains_rate(gains, units.rate = units.year))
      labels[ii] <- "CAGR"
    } else if (metric.ii == "mdd") {
      p.metrics <- cbind(p.metrics, mdd(gains = gains))
      labels[ii] <- "MDD"
    } else if (metric.ii == "sharpe") {
      p.metrics <- cbind(p.metrics, sharpe(gains))
      labels[ii] <- "Sharpe"
    } else if (metric.ii == "sortino") {
      p.metrics <- cbind(p.metrics, sortino(gains))
      labels[ii] <- "Sortino"
    } else if (metric.ii == "alpha") {
      p.metrics <- cbind(p.metrics, 
                         c(0, apply(gains[, -1, drop = F], 2, function(x) 
                           lm(x ~ gains[, 1])$coef[1])))
      labels[ii] <- "Alpha"
    } else if (metric.ii == "beta") {
      p.metrics <- cbind(p.metrics, 
                         c(0, apply(gains[, -1, drop = F], 2, function(x) 
                           lm(x ~ gains[, 1])$coef[2])))
      labels[ii] <- "Beta"
    } else if (metric.ii == "r.squared") {
      p.metrics <- cbind(p.metrics,  
                         c(1, apply(gains[, -1, drop = F], 2, function(x) 
                           summary(lm(x ~ gains[, 1]))$r.squared)))
      labels[ii] <- "R-squared"
    } else if (metric.ii == "pearson") {
      p.metrics <- cbind(p.metrics, 
                         apply(gains, 2, function(x) cor(x, gains[, 1])))
      labels[ii] <- "Pearson r"
    } else if (metric.ii == "spearman") {
      p.metrics <- cbind(p.metrics, 
                         apply(gains, 2, function(x) 
                           cor(x, gains[, 1], method = "spearman")))
      labels[ii] <- "Spearman rho"
    } else if (metric.ii == "auto.pearson") {
      p.metrics <- cbind(p.metrics, 
                         apply(gains, 2, function(x) cor(x[-length(x)], x[-1])))
      labels[ii] <- "Pearson autocor."
    } else if (metric.ii == "auto.spearman") {
      p.metrics <- cbind(p.metrics, 
                         apply(gains, 2, function(x) 
                           cor(x[-length(x)], x[-1], method = "spearman")))
      labels[ii] <- "Spearman autocor."
    }
  }
  
  # Add labels for performance metrics
  colnames(p.metrics) <- labels
  rownames(p.metrics) <- tickers
  
  # Calculate correlation matrix
  cor.mat <- cor(gains)
  
  # Return performance metrics and and correlation matrix
  return.list <- list(perf.metrics = p.metrics, cor.mat = cor.mat)
  return(return.list)
  
}
