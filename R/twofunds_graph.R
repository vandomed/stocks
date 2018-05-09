#' Graph One Performance Metric vs. Another for Two-Fund Portfolios as 
#' Allocation Varies
#' 
#' Useful for visualizing performance of two-fund portfolios, typically by 
#' plotting a measure of growth vs. a measure of volatility. First two 
#' investments are used as the first two-fund pair, next two as the second 
#' two-fund pair, and so on. 
#' 
#' 
#' @inheritParams metrics
#' @inheritParams load_gains
#' 
#' @param benchmark.tickers Character vector of length 1 or 2 indicating ticker 
#' symbols for benchmark indexes. Only used if \code{x.metric} and/or 
#' \code{y.metric} require benchmark indexes to calculate. For example, to plot 
#' correlation with SPY on the x-axis and correlation with TLT on the y-axis, 
#' set \code{x.metric = "pearson"}, \code{y.metric = "pearson2"} (i.e. Pearson 
#' correlation with 2nd benchmark), and 
#' \code{benchmark.tickers = c("SPY", "TLT")}.
#' 
#' @param reference.tickers Character vector of ticker symbols to include on  
#' graph as data points for comparative purposes.
#' 
#' @param tickers.gains Numeric matrix of gains, where each column has gains for 
#' a particular fund.
#' 
#' @param benchmark.gains Numeric vector or matrix of gains for 1 or 2 benchmark 
#' indexes. Only used if \code{x.metric} and/or \code{y.metric} require 
#' benchmark indexes to calculate. For example, to plot correlation with SPY on 
#' the x-axis and correlation with TLT on the y-axis, set 
#' \code{x.metric = "pearson"} and \code{y.metric = "pearson2"}, and 
#' input \code{benchmark.gains} as a 2-column matrix of gains for SPY and TLT.
#' 
#' @param reference.gains Numeric vector or matrix of gains for funds to 
#' include on graph as data points for comparative purposes.
#' 
#' @param step.data Numeric value specifying allocation increments for plotting 
#' curves.
#' 
#' @param step.points Numeric value specifying allocation increments for adding 
#' data points on top of curves. Set to \code{NULL} to suppress data points.
#' 
#' @param x.metric Character string specifying x-axis performance metric. 
#' Choices are: 
#' 
#' \code{"mean"} or \code{"sd"} for mean or standard deviation of gains 
#' 
#' \code{"growth"} or \code{"cagr"} for total or annualized growth 
#' 
#' \code{"mdd"} for maximum drawdown
#' 
#' \code{"sharpe"} or \code{"sortino"} for Sharpe or Sortino ratio 
#' 
#' \code{"alpha"}, \code{"beta"}, or \code{"r.squared"} for those metrics from a 
#' fitted linear regression on benchmark fund
#' 
#' \code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation 
#' with benchmark fund
#' 
#' \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or 
#' \code{"spearman2"} for same as previously described, but using the second 
#' benchmark index
#' 
#' \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman 
#' autocorrelation, defined as the correlation between subsequent gains
#' 
#' \code{"allocation"} for allocation to first fund in each pair.
#' 
#' @param y.metric Same as \code{x.metric}, but for the y-axis
#' 
#' @param tickerlabel.offsets Either a numeric vector of length 2 giving the 
#' x- and y-axis offsets for all ticker labels, or a 2-column matrix where each 
#' row gives the x- and y-axis offsets for a ticker.
#' 
#' @param reflabel.offsets Either a numeric vector of length 2 giving the x- and 
#' y-axis offsets for all reference ticker labels, or a 2-column matrix where 
#' each row gives the x- and y-axis offsets for a reference ticker.
#' 
#' @param add.plot Logical value for whether to add plot data to current plot 
#' frame rather than open a new one.
#' 
#' @param colors Character vector of colors for each curve.
#' 
#' @param lty Numeric vector specifying line types for each curve.
#' 
#' @param plot.list List of arguments to pass to \code{\link[graphics]{plot}}.
#' @param points.list List of arguments to pass to 
#' \code{\link[graphics]{points}}.
#' @param text.list List of arguments to pass to \code{\link[graphics]{text}}.
#' 
#' @param pdf.list List of arguments to pass to \code{\link[grDevices]{pdf}}.
#' @param bmp.list List of arguments to pass to \code{\link[grDevices]{bmp}}.
#' @param jpeg.list List of arguments to pass to \code{\link[grDevices]{jpeg}}.
#' @param png.list List of arguments to pass to \code{\link[grDevices]{png}}.
#' @param tiff.list List of arguments to pass to \code{\link[grDevices]{tiff}}.
#' 
#' 
#' @return
#' In addition to the graph, a list containing: 
#' \enumerate{
#' \item List named \code{portfolio.xy} where each element is a two-column 
#' matrix of x- and y-axis values for a fund pair.
#' \item Numeric vector named \code{means} with mean gains for each fund.
#' \item Numeric matrix named \code{corr.matrix} with a correlation matrix for 
#' gains for each fund.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot mean vs. SD for UPRO/VBLTX portfolio, and compare to VFINX and BRK-B
#' fig1 <- twofunds_graph(tickers = c("UPRO", "VBLTX"), 
#'                        reference.tickers = c("VFINX", "BRK-B"))
#'                      
#' # Same funds, but annualized growth vs. maximum drawdown
#' fig2 <- twofunds_graph(tickers = c("UPRO", "VBLTX"), 
#'                        reference.tickers = c("VFINX", "BRK-B"),
#'                        x.metric = "mdd", y.metric = "cagr")
#' }
#'
#' @export
twofunds_graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                           ...,
                           benchmark.tickers = NULL,
                           reference.tickers = NULL,
                           tickers.gains = NULL,
                           benchmark.gains = NULL,
                           reference.gains = NULL,
                           step.data = 0.0025,
                           step.points = 0.1,
                           x.metric = "sd",
                           y.metric = "mean",
                           tickerlabel.offsets = NULL,
                           reflabel.offsets = NULL,
                           add.plot = FALSE,
                           colors = NULL,
                           lty = NULL,
                           plot.list = NULL,
                           points.list = NULL,
                           text.list = NULL,
                           pdf.list = NULL,
                           bmp.list = NULL,
                           jpeg.list = NULL,
                           png.list = NULL,
                           tiff.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Get number of tickers
    n.tickers <- length(tickers)
    n.pairs <- n.tickers / 2
    n.bench <- length(benchmark.tickers)
    n.ref <- length(reference.tickers)
    n.extra <- n.bench + n.ref
    
    # Create vector of "extra" tickers
    extra.tickers <- unique(c(benchmark.tickers, reference.tickers))
    
    # If intercepts or slopes NULL, set to 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- rep(0, n.tickers)
    }
    if (is.null(slopes)) {
      slopes <- rep(1, n.tickers)
    }
    
    # Calculate gains matrix
    tickers.vec <- c(tickers, extra.tickers)
    intercepts.vec <- c(intercepts, rep(0, n.extra))
    slopes.vec <- c(slopes, rep(1, n.extra))
    gains <- load_gains(tickers = tickers.vec, intercepts = intercepts.vec,
                        slopes = slopes.vec, ...)
    
    # Update ticker names to show intercept/slope
    tickers <- colnames(gains)[1: n.tickers]
    
    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: n.tickers, drop = F]
    extra.gains <- gains[, -c(1: n.tickers), drop = F]
    if (n.bench > 0) {
      benchmark.gains <- extra.gains[, 1: n.bench, drop = F]
      extra.gains <- extra.gains[, -c(1: n.bench), drop = F]
    }
    if (n.ref > 0) {
      reference.gains <- extra.gains
    }
    
  } else {
    
    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    n.tickers <- length(tickers)
    n.pairs <- n.tickers / 2
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: n.tickers, sep = "")
    }
    
    # Convert reference.gains to matrix if necessary, and figure out
    # reference.tickers
    if (is.vector(reference.gains)) {
      reference.gains <- matrix(reference.gains, ncol = 1)
      reference.tickers <- "REF"
    } else if (is.matrix(reference.gains)) {
      reference.tickers <- colnames(reference.gains)
      if (is.null(reference.tickers)) {
        reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
      }
    }
    
    # Convert benchmark.gains to matrix if necessary, and figure out
    # benchmark.tickers
    if (is.vector(benchmark.gains)) {
      benchmark.gains <- matrix(benchmark.gains, ncol = 1)
      benchmark.tickers <- "BENCH"
    } else if (is.matrix(benchmark.gains)) {
      benchmark.tickers <- colnames(benchmark.gains)
      if (is.null(benchmark.tickers)) {
        benchmark.tickers <- paste("BENCH", 1: ncol(benchmark.gains), sep = "")
      }
    }
    
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
    min.diffdates <- min(diff(as.Date(rownames(tickers.gains)
                                      [1: min(10, nrow(tickers.gains))])))
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
  
  # Initialize list to store x-axis values and y-axis values for each pair
  x <- y <- portfolio.xy <- list()
  
  # Loop through fund pairs
  fund1.all <- seq(0, 1, step.data)
  fund2.all <- 1 - fund1.all
  fund.all <- cbind(fund1.all, fund2.all)
  num.points <- length(fund1.all)
  
  for (ii in 1: n.pairs) {
    
    # Get subset of tickers.gains matrix for tickers of interest
    columns <- c(ii * 2 - 1, ii * 2)
    tickers.gains.sub <- tickers.gains[, columns]
    
    # Calculate x-axis value for each allocation
    if (x.metric == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      x[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100
      
    } else if (x.metric == "sd") {
      
      vars <- var(tickers.gains.sub)
      x[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                        fund1.all * fund2.all * vars[1, 2]) * 100
      
    } else if (x.metric == "growth") {
      
      x[[ii]] <- apply(fund.all, 1, function(x)
        gains_rate(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (x.metric == "cagr") {
      
      x[[ii]] <- apply(fund.all, 1, function(x)
        gains_rate(gains = tickers.gains.sub %*% x,
                   units.rate = units.year)) * 100
      
    } else if (x.metric == "mdd") {
      
      x[[ii]] <- apply(fund.all, 1, function(x) 
        mdd(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (x.metric == "sharpe") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      x1 <- fund.all %*% means
      vars <- var(tickers.gains.sub)
      x2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                   2 * fund1.all * fund2.all * vars[1, 2])
      x[[ii]] <- x1 / x2
      
    } else if (x.metric == "sortino") {
      
      x[[ii]] <- apply(fund.all, 1, function(x) 
        sortino(gains = tickers.gains.sub %*% x))
      
    } else if (x.metric == "alpha") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      x[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (x.metric == "alpha2") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      x[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (x.metric == "beta") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      x[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (x.metric == "beta2") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      x[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (x.metric == "r.squared") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      x[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                            2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3]))^2
      
    } else if (x.metric == "r.squared2") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      x[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                            2 * fund1.all * fund2.all * vars[1, 2]) *
                           vars[3, 3]))^2
      
    } else if (x.metric == "pearson") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      x[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])
      
    } else if (x.metric == "pearson2") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      x[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])
      
    } else if (x.metric == "spearman") {
      
      x[[ii]] <- apply(fund.all, 1, function(x)
        cor(tickers.gains.sub %*% x, benchmark.gains[, 1], method = "spearman"))
      
    } else if (x.metric == "spearman2") {
      
      x[[ii]] <- apply(fund.all, 1, function(x)
        cor(tickers.gains.sub %*% x, benchmark.gains[, 2], method = "spearman"))
      
    } else if (x.metric == "auto.pearson") {
      
      vars <- var(cbind(tickers.gains.sub[1: (nrow(tickers.gains.sub) - 1), ],
                        tickers.gains.sub[2: nrow(tickers.gains.sub), ]))
      x[[ii]] <- (fund1.all^2 * vars[1, 3] +
                    fund1.all * fund2.all * vars[1, 4] +
                    fund1.all * fund2.all * vars[2, 3] +
                    fund2.all^2 * vars[2, 4]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) *
               (fund1.all^2 * vars[3, 3] + fund2.all^2 * vars[4, 4] +
                  2 * fund1.all * fund2.all * vars[3, 4]))
      
    } else if (x.metric == "auto.spearman") {
      
      num.gains <- nrow(tickers.gains.sub)
      x[[ii]] <- apply(fund.all, 1, function(x)
        cor((tickers.gains.sub %*% x)[-num.gains],
            (tickers.gains.sub %*% x)[-1], method = "spearman"))
      
    } else if (x.metric == "allocation") {
      
      x[[ii]] <- fund1.all * 100
      
    }
    
    # Calculate y-axis value for each allocation
    if (y.metric == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      y[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100
      
    } else if (y.metric == "sd") {
      
      vars <- var(tickers.gains.sub)
      y[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                        2 * fund1.all * fund2.all * vars[1, 2]) * 100
      
    } else if (y.metric == "growth") {
      
      y[[ii]] <- apply(fund.all, 1, function(x)
        gains_rate(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (y.metric == "cagr") {
      
      y[[ii]] <- apply(fund.all, 1, function(x)
        gains_rate(gains = tickers.gains.sub %*% x,
                   units.rate = units.year)) * 100
      
    } else if (y.metric == "mdd") {
      
      y[[ii]] <- apply(fund.all, 1, function(x)
        mdd(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (y.metric == "sharpe") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      y1 <- fund1.all * means[1] + fund2.all * means[2]
      vars <- var(tickers.gains.sub)
      y2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                   2 * fund1.all * fund2.all * vars[1, 2])
      y[[ii]] <- y1 / y2
      
    } else if (y.metric == "sortino") {
      
      y[[ii]] <- apply(fund.all, 1, function(x) 
        sortino(gains = tickers.gains.sub %*% x))
      
    } else if (y.metric == "alpha") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      y[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (y.metric == "alpha2") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      y[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (y.metric == "beta") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      y[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (y.metric == "beta2") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      y[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (y.metric == "r.squared") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      y[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                            2 * fund1.all * fund2.all * vars[1, 2]) *
                           vars[3, 3]))^2
      
    } else if (y.metric == "r.squared2") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      y[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                            2 * fund1.all * fund2.all * vars[1, 2]) *
                           vars[3, 3]))^2
      
    } else if (y.metric == "pearson") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      y[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])
      
    } else if (y.metric == "pearson2") {
      
      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      y[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])
      
    } else if (y.metric == "spearman") {
      
      y[[ii]] <- apply(fund.all, 1, function(x)
        cor(tickers.gains.sub %*% x, benchmark.gains[, 1], method = "spearman"))
      
    } else if (y.metric == "spearman2") {
      
      y[[ii]] <- apply(fund.all, 1, function(x)
        cor(tickers.gains.sub %*% x, benchmark.gains[, 2], method = "spearman"))
      
    } else if (y.metric == "auto.pearson") {
      
      vars <- var(cbind(tickers.gains.sub[1: (nrow(tickers.gains.sub) - 1), ],
                        tickers.gains.sub[2: nrow(tickers.gains.sub), ]))
      y[[ii]] <- (fund1.all^2 * vars[1, 3] +
                    fund1.all * fund2.all * vars[1, 4] +
                    fund1.all * fund2.all * vars[2, 3] +
                    fund2.all^2 * vars[2, 4]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] +
                2 * fund1.all * fund2.all * vars[1, 2]) *
               (fund1.all^2 * vars[3, 3] + fund2.all^2 * vars[4, 4] +
                  2 * fund1.all * fund2.all * vars[3, 4]))
      
    } else if (y.metric == "auto.spearman") {
      
      num.gains <- nrow(tickers.gains.sub)
      y[[ii]] <- apply(fund.all, 1, function(x)
        cor((tickers.gains.sub %*% x)[-num.gains],
            (tickers.gains.sub %*% x)[-1], method = "spearman"))
      
    } else if (y.metric == "allocation") {
      
      y[[ii]] <- fund1.all * 100
      
    }
    
    # Combine x and y values into two-column matrix
    portfolio.xy[[ii]] <- cbind(x[[ii]], y[[ii]])
    
  }
  
  # Create variables for plot
  x1 <- x2 <- y1 <- y2 <- NULL
  reference.y <- NULL
  if (y.metric == "mean") {
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ",
                        sep = "")
    y.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, mean) * 100
    }
  } else if (y.metric == "sd") {
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ",
                        sep = "")
    y.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, sd) * 100
    }
    y1 <- 0
  } else if (y.metric == "growth") {
    plot.title <- "Total Growth vs. "
    y.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        gains_rate(gains = x)) * 100
    }
  } else if (y.metric == "cagr") {
    plot.title <- "CAGR vs. "
    y.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        gains_rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (y.metric == "mdd") {
    plot.title <- "MDD vs. "
    y.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    y1 <- 0
  } else if (y.metric == "sharpe") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe(gains = x))
    }
  } else if (y.metric == "sortino") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sortino(gains = x))
    }
  } else if (y.metric == "alpha") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 1])$coef[1]) * 100
    }
  } else if (y.metric == "alpha2") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 2])$coef[1]) * 100
    }
  } else if (y.metric == "beta") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 1])$coef[2])
    }
  } else if (y.metric == "beta2") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 2])$coef[2])
    }
  } else if (y.metric == "r.squared") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "r.squared2") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "pearson") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 1]))
    }
  } else if (y.metric == "pearson2") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 2]))
    }
  } else if (y.metric == "spearman") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 1], method = "spearman"))
    }
  } else if (y.metric == "spearman2") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 2], method = "spearman"))
    }
  } else if (y.metric == "auto.pearson") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x[-length(x)], x[-1]))
    }
  } else if (y.metric == "auto.spearman") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x)
        cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (y.metric == "allocation") {
    plot.title <- "Allocation vs. "
    y.label <- "Allocation (%)"
    y1 <- -5
    y2 <- 105
  }
  
  reference.x <- NULL
  if (x.metric == "mean") {
    plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale),
                        " Gains", sep = "")
    x.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, mean) * 100
    }
  } else if (x.metric == "sd") {
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale),
                        " Gains", sep = "")
    x.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, sd) * 100
    }
    x1 <- 0
  } else if (x.metric == "growth") {
    plot.title <- paste(plot.title, "Total Growth", sep = "")
    x.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        gains_rate(gains = x)) * 100
    }
  } else if (x.metric == "cagr") {
    plot.title <- paste(plot.title, "CAGR", sep = "")
    x.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        gains_rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (x.metric == "mdd") {
    plot.title <- paste(plot.title, "MDD", sep = "")
    x.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    x1 <- 0
  } else if (x.metric == "sharpe") {
    plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
    x.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe(gains = x))
    }
  } else if (x.metric == "sortino") {
    plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
    x.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sortino(gains = x))
    }
  } else if (x.metric == "alpha") {
    plot.title <- paste(plot.title, "Alpha")
    x.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.metric == "alpha2") {
    plot.title <- paste(plot.title, "Alpha")
    x.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.metric == "beta") {
    plot.title <- paste(plot.title, "Beta")
    x.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.metric == "beta2") {
    plot.title <- paste(plot.title, "Beta")
    x.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.metric == "r.squared") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains))
    }
  } else if (x.metric == "pearson2") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains, method = "spearman"))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains, method = "spearman"))
    }
  } else if (x.metric == "auto.pearson") {
    plot.title <- paste(plot.title, "Autocorrelation", "")
    x.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x[-length(x)], x[-1]))
    }
  } else if (x.metric == "auto.spearman") {
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (x.metric == "allocation") {
    plot.title <- paste(plot.title, "Allocation")
    x.title <- "Allocation"
    x.label <- "Allocation (%)"
    x1 <- -5
    x2 <- 105
  }
  
  # If NULL, set appropriate values for xlim and ylim ranges
  if (is.null(x1) | is.null(x2) | is.null(y1) | is.null(y2)) {
    xvals <- c(unlist(x), reference.x)
    xvals.range <- range(xvals)
    yvals <- c(unlist(y), reference.y)
    yvals.range <- range(yvals)
    if (is.null(x1)) {
      x1 <- xvals.range[1] - 0.05 * diff(xvals.range)
    }
    if (is.null(x2)) {
      x2 <- xvals.range[2] + 0.05 * diff(xvals.range)
    }
    if (is.null(y1)) {
      y1 <- yvals.range[1] - 0.05 * diff(yvals.range)
    }
    if (is.null(y2)) {
      y2 <- yvals.range[2] + 0.05 * diff(yvals.range)
    }
  }
  
  # Create color scheme for plot
  if (is.null(colors)) {
    if (n.pairs == 1) {
      colors <- "black"
    } else if (n.pairs == 2) {
      colors <- c("blue", "red")
    } else if (n.pairs == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.pairs == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.pairs > 4) {
      colors <- colorRampPalette(c("blue", "red"))(n.pairs)
    }
  }
  if (is.null(lty)) {
    lty <- rep(1, n.pairs)
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- list_override(list1 = list(x = 0, y = 0, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list_override(list1 = list(pch = 16, cex = 0.6),
                               list2 = points.list)
  text.list <- list_override(list1 = list(cex = 0.7),
                             list2 = text.list)
  
  # Figure out positioning of ticker labels for 100% allocation to each fund
  if (is.null(tickerlabel.offsets)) {
    
    tickerlabel.offsets <- cbind(rep(0, n.tickers), rep(NA, n.tickers))
    y.offset.mag <- (y2 - y1) / 40
    for (ii in 1: n.pairs) {
      
      # Put label for ticker with higher y-value above its data point, and
      # label for other ticker below its data point
      fund1.xy <- c(x[[ii]][num.points], y[[ii]][num.points])
      fund2.xy <- c(x[[ii]][1], y[[ii]][1])
      whichmax.y <- which.max(c(fund1.xy[2], fund2.xy[2]))
      if (whichmax.y == 1) {
        tickerlabel.offsets[(ii * 2 - 1), 2] <- y.offset.mag
        tickerlabel.offsets[ii * 2, 2] <- -y.offset.mag
      } else {
        tickerlabel.offsets[(ii * 2 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 2, 2] <- y.offset.mag
      }
    }
    
  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.pairs * 2),
                                 rep(tickerlabel.offsets[2], n.pairs * 2))
  }
  if (is.null(reflabel.offsets) & !is.null(reference.tickers)) {
    reflabel.offsets <- cbind(rep(0, length(reference.tickers)),
                              rep((y2 - y1) / 40, length(reference.tickers)))
  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.pairs * 2),
                                 rep(tickerlabel.offsets[2], n.pairs * 2))
  }
  
  # If pdf.list is not NULL, call pdf
  if (! is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }
  
  # If bmp.list is not NULL, call bmp
  if (! is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }
  
  # If jpeg.list is not NULL, call jpeg
  if (! is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }
  
  # If png.list is not NULL, call png
  if (! is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }
  
  # If tiff.list is not NULL, call tiff
  if (! is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }
  
  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }
  
  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2",
                      "beta", "beta2", "pearson", "pearson2", "spearman",
                      "spearman2", "auto.pearson", "auto.spearman", "growth",
                      "cagr")) {
    abline(h = 0, lty = 2)
  } else if (y.metric %in% c("r.squared", "r.squared2")) {
    abline(h = 1, lty = 2)
  }
  if (x.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2",
                      "beta", "beta2", "pearson", "pearson2", "spearman",
                      "spearman2", "auto.pearson", "auto.spearman", "growth",
                      "cagr")) {
    abline(v = 0, lty = 2)
  } else if (x.metric %in% c("r.squared", "r.squared2")) {
    abline(v = 1, lty = 2)
  }
  
  # Figure out indices for data points
  if (!is.null(step.points)) {
    locs.points <- seq(1, num.points, step.points / step.data)
  } else {
    locs.points <- c(1, num.points)
  }
  
  # Add curves for each pair
  for (ii in 1: n.pairs) {
    
    # Add colored curves and data points
    do.call(points, c(list(x = x[[ii]], y = y[[ii]], type = "l",
                           col = colors[ii], lty = lty[ii]), points.list))
    do.call(points, c(list(x = x[[ii]][locs.points], y = y[[ii]][locs.points],
                           col = colors[ii]), points.list))
    
    # Figure out (x, y) coordinates for 100% fund 1 and 100% fund 2
    fund1.xy <- c(x[[ii]][num.points], y[[ii]][num.points])
    fund2.xy <- c(x[[ii]][1], y[[ii]][1])
    
    # Add black data points at 100% fund 1 and 100% fund2
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))
    
    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[ii * 2 - 1] %in% tickers[1: (ii * 2 - 2)]) {
      do.call(text, c(list(x = fund1.xy[1] +
                             tickerlabel.offsets[(ii * 2 - 1), 1],
                           y = fund1.xy[2] +
                             tickerlabel.offsets[(ii * 2 - 1), 2],
                           label = paste("100% ", tickers[ii * 2 - 1], sep = "")),
                      text.list))
    }
    if (ii == 1 | ! tickers[ii * 2] %in% tickers[1: (ii * 2 - 2)]) {
      do.call(text, c(list(x = fund2.xy[1] + tickerlabel.offsets[ii * 2, 1],
                           y = fund2.xy[2] + tickerlabel.offsets[ii * 2, 2],
                           label = paste("100% ", tickers[ii * 2], sep = "")),
                      text.list))
    }
    
  }
  
  # Add data point for reference funds (if given)
  if (! is.null(reference.tickers)) {
    
    # Loop through and add data points for each reference fund
    for (ii in 1: ncol(reference.gains)) {
      
      if (x.metric != "allocation" & y.metric != "allocation") {
        
        do.call(points, c(list(x = reference.x[ii], y = reference.y[ii],
                               type = "p", col = "black"), points.list))
        if (! reference.tickers[ii] %in% tickers) {
          do.call(text, c(list(x = reference.x[ii] + reflabel.offsets[ii, 1],
                               y = reference.y[ii] + reflabel.offsets[ii, 2],
                               label = reference.tickers[ii]),
                          text.list))
          
        }
      } else {
        
        if (y.metric == "allocation") {
          
          abline(v = reference.x[ii], lty = 2, col = "black")
          do.call(text, c(list(x = reference.x[ii] + reflabel.offsets[ii, 1],
                               y = 20,
                               label = reference.tickers[ii]),
                          text.list))
          
        } else {
          
          abline(h = reference.y[ii], lty = 2, col = "black")
          do.call(text, c(list(x = 20,
                               y = reference.y[ii] + reflabel.offsets[ii, 2],
                               label = reference.tickers[ii]),
                          text.list))
          
        }
      }
    }
    
  }
  
  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }
  
  # Return portfolio.xy, mean for each fund and correlation matrix
  if (! exists("gains")) {
    gains <- cbind(tickers.gains, benchmark.gains, reference.gains)
  }
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(portfolio.xy = portfolio.xy, means = means,
                      corr.matrix = corr.matrix)
  return(return.list)
  
}
