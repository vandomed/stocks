#' Graph Performance Metric Over Time for Various Investments
#' 
#' Useful for visualizing the performance of a group of investments over time. 
#' The first investment is used as the benchmark if the requested metric 
#' requires one.
#' 
#' 
#' 
#' @inheritParams onemetric_graph
#' @inheritParams twofunds_graph
#' 
#' @param window.units Numeric value specifying the width of the moving window.
#' @param legend.list List of arguments to pass to 
#' \code{\link[graphics]{legend}}.
#' 
#' @return
#' In addition to the graph, a numeric matrix containing the performance metric 
#' over time for each investment.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot BRK-B's 50-day alpha over time since the start of 2016
#' fig <- onemetric_overtime_graph(tickers = c("VFINX", "BRK-B"), 
#'                                 y.metric = "alpha", 
#'                                 from = "2016-01-01")
#' }
#'
#' @export
onemetric_overtime_graph <- function(tickers = NULL, ...,
                                     gains = NULL,
                                     prices = NULL,
                                     y.metric = "cagr",
                                     window.units = 50,
                                     add.plot = FALSE,
                                     colors = NULL,
                                     lty = NULL, 
                                     plot.list = NULL,
                                     points.list = NULL,
                                     legend.list = NULL,
                                     pdf.list = NULL,
                                     bmp.list = NULL,
                                     jpeg.list = NULL,
                                     png.list = NULL,
                                     tiff.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Obtain matrix of gains for each fund
    gains <- load_gains(tickers = tickers, ...)
    
  } else if (! is.null(prices)) {
    
    # Calculate gains based on price data
    gains <- prices_gains(prices = prices)
    
  } else if (is.null(gains)) {
    
    stop("You must specify 'tickers', 'gains', or 'prices'")
    
  }
  
  # Convert gains to matrix if not already
  if (! is.matrix(gains)) {
    gains <- matrix(gains, ncol = 1)
  }
  
  # If y.metric requires a benchmark, split gains matrix into ticker gains and
  # benchmark gains
  if (y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
    benchmark.gains <- gains[, 1, drop = F]
    benchmark.ticker <- colnames(benchmark.gains)
    if (is.null(benchmark.ticker)) {
      benchmark.ticker <- "BENCH"
    }
    gains <- gains[, -1, drop = F]
  }
  
  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2,
  # ...
  tickers <- colnames(gains)
  n.tickers <- length(tickers)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: n.tickers)
  }
  
  # Get dates
  rows <- rownames(gains)[-c(1: (window.units - 1))]
  if (! is.null(rows)) {
    dates <- as.Date(rows)
  } else {
    dates <- 1: (nrow(gains) - window.units + 1)
  }
  if (y.metric %in% c("auto.pearson", "auto.spearman")) {
    dates <- dates[-1]
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
    min.diffdates <- min(diff(as.Date(rownames(gains)
                                      [1: min(10, nrow(gains))])))
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
  
  # Calculate performance metrics
  y1 <- y2 <- NULL
  if (y.metric == "mean") {
    y <- rollapply(gains, width = window.units,
                   FUN = mean, by.column = TRUE) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- rollapply(gains, width = window.units,
                   FUN = sd, by.column = TRUE) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Standard deviation (%)"
    y1 <- 0
  } else if (y.metric == "growth") {
    y <- rollapply(gains, width = window.units,
                   FUN = function(x)
                     gains_rate(gains = x) * 100, by.column = TRUE)
    plot.title <- "Total Growth"
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    y <- rollapply(gains, width = window.units,
                   FUN = function(x)
                     gains_rate(gains = x, units.rate = units.year) * 100,
                   by.column = TRUE)
    plot.title <- "Compound Annualized Growth Rate"
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- rollapply(gains, width = window.units,
                   FUN = function(x) mdd(gains = x) * 100, by.column = TRUE)
    plot.title <- "Maximum Drawdown"
    y.label <- "MDD (%)"
    y1 <- 0
  } else if (y.metric == "sharpe") {
    y <- rollapply(gains, width = window.units, FUN = sharpe, by.column = TRUE)
    plot.title <- "Sharpe Ratio"
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- rollapply(gains, width = window.units, FUN = sortino, by.column = TRUE)
    plot.title <- "Sortino Ratio"
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- matrix(NA, ncol = n.tickers, nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: n.tickers) {
        y[(ii - window.units + 1), jj] <-
          lm(gains[locs, jj] ~ benchmark.gains[locs])$coef[1] * 100
      }
    }
    plot.title <- paste("Alpha w/ ", benchmark.ticker, sep = "")
    y.label <- "Alpha (%)"
  } else if (y.metric == "beta") {
    y <- matrix(NA, ncol = n.tickers, nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: n.tickers) {
        y[(ii - window.units + 1), jj] <-
          lm(gains[locs, jj] ~ benchmark.gains[locs])$coef[2]
      }
    }
    plot.title <- paste("Beta w/ ", benchmark.ticker, sep = "")
    y.label <- "Beta"
  } else if (y.metric == "r.squared") {
    y <- matrix(NA, ncol = n.tickers, nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: n.tickers) {
        y[(ii - window.units + 1), jj] <-
          summary(lm(gains[locs, jj] ~ benchmark.gains[locs]))$r.squared
      }
    }
    plot.title <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y.label <- "R-squared"
    y1 <- 0
  } else if (y.metric == "pearson") {
    y <- matrix(NA, ncol = n.tickers, nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: n.tickers) {
        y[(ii - window.units + 1), jj] <- cor(gains[locs, jj],
                                              benchmark.gains[locs])
      }
    }
    plot.title <- paste("Pearson Cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Pearson correlation"
  } else if (y.metric == "spearman") {
    y <- matrix(NA, ncol = n.tickers, nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: n.tickers) {
        y[(ii - window.units + 1), jj] <- cor(gains[locs, jj],
                                              benchmark.gains[locs],
                                              method = "spearman")
      }
    }
    plot.title <- paste("Spearman Cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Spearman correlation"
  } else if (y.metric == "auto.pearson") {
    y <- rollapply(gains, width = window.units + 1,
                   FUN = function(x)
                     cor(x[-length(x)], x[-1]), by.column = TRUE)
    plot.title <- "Autocorrelation"
    y.label <- paste("Pearson cor. for adjacent ", time.scale, " gains",
                     sep = "")
  } else if (y.metric == "auto.spearman") {
    y <- rollapply(gains, width = window.units + 1,
                   FUN = function(x)
                     cor(x[-length(x)], x[-1], method = "spearman"),
                   by.column = TRUE)
    plot.title <- "Autocorrelation"
    y.label <- paste("Spearman cor. for adjacent ", time.scale, " gains",
                     sep = "")
  }
  
  # If NULL, set appropriate values for ylim range
  if (is.null(y1)) {
    y1 <- min(y) * ifelse(min(y) > 0, 0.95, 1.05)
  }
  if (is.null(y2)) {
    y2 <- max(y) * ifelse(max(y) > 0, 1.05, 0.95)
  }
  
  # Create color scheme for plot
  if (is.null(colors)) {
    if (n.tickers == 1) {
      colors <- "black"
    } else if (n.tickers == 2) {
      colors <- c("blue", "red")
    } else if (n.tickers == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.tickers == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.tickers > 4) {
      #colors <- distinctColorPalette(n.tickers)
      colors <- colorRampPalette(c("blue", "red", "darkgreen"))(n.tickers)
    }
  }
  if (is.null(lty)) {
    lty <- rep(1, n.tickers)
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- list_override(list1 = list(x = dates,
                                          y = y[, 1], type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = "Date", ylab = y.label,
                                          ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list_override(list1 = list(pch = 16),
                               list2 = points.list)
  legend.list <- list_override(list1 = list(x = "topleft", lty = lty,
                                            col = colors, legend = tickers),
                               list2 = legend.list)
  
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
  if (y.metric %in% c("mean", "sd", "growth", "cagr", "sharpe", "sortino",
                      "alpha", "beta", "pearson", "spearman", "auto.pearson",
                      "auto.spearman")) {
    abline(h = 0, lty = 2)
  } else if (y.metric == "r.squared") {
    abline(h = 1, lty = 2)
  }
  
  # Add curves for each fund
  for (ii in 1: n.tickers) {
    
    # Add colored curves and data points
    do.call(points, c(list(x = dates, y = y[, ii], type = "l",
                           col = colors[ii], lty = lty[ii]), points.list))
    
  }
  
  # Add legend
  if (length(tickers) > 1) {
    do.call(legend, legend.list)
  }
  
  # Close graphics device if necessary
  if (! is.null(pdf.list) | ! is.null(bmp.list) | ! is.null(jpeg.list) |
      ! is.null(png.list) | ! is.null(tiff.list)) {
    dev.off()
  }
  
  # Return matrix of y values
  colnames(y) <- tickers
  return(y)
  
}
