#' Graph One Performance Metric vs. Another for Various Investments
#' 
#' Useful for visualizing the performance of a group of investments. The first 
#' investment is used as the benchmark if \code{x.metric} or \code{y.metric} 
#' require one benchmark, and the first two investments are used as benchmarks 
#' if \code{x.metric} and \code{y.metric} require different benchmarks.
#' 
#' 
#' @inheritParams onemetric_graph
#' @inheritParams twofunds_graph
#' 
#' @param x.metric Character string specifying x-axis performance metric. 
#' Choices are:
#' 
#' \code{"mean"} or \code{"sd"} for mean or standard deviation of gains.
#' 
#' \code{"growth"} or \code{"cagr"} for total or annualized growth.
#' 
#' \code{"mdd"} for maximum drawdown.
#' 
#' \code{"sharpe"} or \code{"sortino"} for Sharpe or Sortino ratio.
#' 
#' \code{"alpha"}, \code{"beta"}, or \code{"r.squared"} for those metrics from a 
#' fitted linear regression on benchmark fund.
#' 
#' \code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation 
#' with benchmark fund.
#' 
#' \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or 
#' \code{"spearman2"} for same as previously described, but using the second 
#' benchmark index.
#' 
#' \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman 
#' autocorrelation, defined as the correlation between subsequent gains.
#' 
#' @param y.metric Same as \code{x.metric}, but for the y-axis.
#'
#' 
#' 
#' @return
#' In addition to the graph, a data frame containing the performance metrics for 
#' each investment.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot annualized growth vs. maximum drawdown for VFINX, SSO, and UPRO
#' fig <- twometrics_graph(tickers = c("VFINX", "SSO", "UPRO"))
#' }
#'
#' @export
twometrics_graph <- function(tickers = NULL, ...,
                             gains = NULL,
                             prices = NULL,
                             x.metric = "mdd",
                             y.metric = "cagr",
                             tickerlabel.offsets = NULL,
                             add.plot = FALSE,
                             colors = NULL,
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
  
  # If x.metric or y.metric requires one or two benchmarks, split gains matrix
  # into ticker gains and benchmark gains
  if (x.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman") |
      y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
    benchmark.gains <- gains[, 1, drop = F]
    benchmark.ticker <- colnames(benchmark.gains)
    if (is.null(benchmark.ticker)) {
      benchmark.ticker <- "BENCH"
    }
    gains <- gains[, -1, drop = F]
  }
  if (x.metric %in%
      c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2") |
      y.metric %in%
      c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2")) {
    benchmark2.gains <- gains[, 1, drop = F]
    benchmark2.ticker <- colnames(benchmark2.gains)
    if (is.null(benchmark2.ticker)) {
      benchmark2.ticker <- "BENCH 2"
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
  x1 <- x2 <- y1 <- y2 <- NULL
  if (y.metric == "mean") {
    y <- apply(gains, 2, mean) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ",
                        sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- apply(gains, 2, sd) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ",
                        sep = "")
    y.label <- "Standard deviation (%)"
    y1 <- 0
  } else if (y.metric == "growth") {
    y <- apply(gains, 2, function(x) gains_rate(gains = x)) * 100
    plot.title <- "Total Growth vs. "
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    y <- apply(gains, 2, function(x)
      gains_rate(gains = x, units.rate = units.year)) * 100
    plot.title <- "CAGR vs. "
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- "Maximum Drawdown vs. "
    y.label <- "MDD (%)"
    y1 <- 0
  } else if (y.metric == "sharpe") {
    y <- apply(gains, 2, function(x) sharpe(gains = x))
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- apply(gains, 2, function(x) sortino(gains = x))
    plot.title <- "Sortino Ratio vs. "
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
  } else if (y.metric == "alpha2") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
  } else if (y.metric == "beta") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
  } else if (y.metric == "beta2") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
  } else if (y.metric == "r.squared") {
    y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y1 <- 0
  } else if (y.metric == "r.squared2") {
    y <- apply(gains, 2, function(x)
      summary(lm(x ~ benchmark2.gains))$r.squared)
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
    y1 <- 0
  } else if (y.metric == "pearson") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "pearson2") {
    y <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "spearman") {
    y <- apply(gains, 2, function(x)
      cor(x, benchmark.gains, method = "spearman"))
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "spearman2") {
    y <- apply(gains, 2, function(x)
      cor(x, benchmark2.gains, method = "spearman"))
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "auto.pearson") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- "Autocorrelation vs. "
    y.label <- "Pearson autocorrelation"
  } else if (y.metric == "auto.spearman") {
    y <- apply(gains, 2, function(x)
      cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- "Autocorrelation vs. "
    y.label <- "Spearman autocorrelation"
  }
  
  if (x.metric == "mean") {
    x <- apply(gains, 2, mean) * 100
    plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale),
                        " Gains", sep = "")
    x.label <- "Mean (%)"
  } else if (x.metric == "sd") {
    x <- apply(gains, 2, sd) * 100
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale),
                        " Gains", sep = "")
    x.label <- "Standard deviation (%)"
    x1 <- 0
  } else if (x.metric == "growth") {
    x <- apply(gains, 2, function(x) gains_rate(gains = x) * 100)
    plot.title <- paste(plot.title, "Total Growth", sep = "")
    x.label <- "CAGR (%)"
  } else if (x.metric == "cagr") {
    units.year <- ifelse(time.scale == "daily", 252,
                         ifelse(time.scale == "monthly", 12,
                                1))
    x <- apply(gains, 2, function(x)
      gains_rate(gains = x, units.rate = units.year) * 100)
    plot.title <- paste(plot.title, "CAGR", sep = "")
    x.label <- "CAGR (%)"
  } else if (x.metric == "mdd") {
    x <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- paste(plot.title, "MDD", sep = "")
    x.label <- "MDD (%)"
    x1 <- 0
  } else if (x.metric == "sharpe") {
    x <- apply(gains, 2, function(x) sharpe(gains = x))
    plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
    x.label <- "Sharpe ratio"
  } else if (x.metric == "sortino") {
    x <- apply(gains, 2, function(x) sortino(gains = x))
    plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
    x.label <- "Sortino ratio"
  } else if (x.metric == "alpha") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
  } else if (x.metric == "alpha2") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
  } else if (x.metric == "beta") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
  } else if (x.metric == "beta2") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
  } else if (x.metric == "r.squared") {
    x <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    x <- apply(gains, 2, function(x)
      summary(lm(x ~ benchmark2.gains))$r.squared)
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
    x1 <- 0
  } else if (x.metric == "pearson") {
    x <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "pearson2") {
    x <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "spearman") {
    x <- apply(gains, 2, function(x)
      cor(x, benchmark.gains, method = "spearman"))
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "spearman2") {
    x <- apply(gains, 2, function(x)
      cor(x, benchmark2.gains, method = "spearman"))
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "auto.pearson") {
    x <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Pearson autocorrelation"
  } else if (x.metric == "auto.spearman") {
    x <- apply(gains, 2, function(x)
      cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Spearman autocorrelation"
  }
  
  # If NULL, set appropriate values for xlim and ylim ranges
  if (is.null(x1) | is.null(x2) | is.null(y1) | is.null(y2)) {
    x.range <- range(x)
    y.range <- range(y)
    if (is.null(x1)) {
      x1 <- x.range[1] - 0.05 * diff(x.range)
    }
    if (is.null(x2)) {
      x2 <- x.range[2] + 0.05 * diff(x.range)
    }
    if (is.null(y1)) {
      y1 <- y.range[1] - 0.05 * diff(y.range)
    }
    if (is.null(y2)) {
      y2 <- y.range[2] + 0.05 * diff(y.range)
    }
  }
  
  # Create color scheme for plot
  if (is.null(colors)) {
    colors <- "black"
    # if (n.tickers == 1) {
    #   colors <- "black"
    # } else if (n.tickers == 2) {
    #   colors <- c("blue", "red")
    # } else if (n.tickers == 3) {
    #   colors <- c("blue", "red", "orange")
    # } else if (n.tickers == 4) {
    #   colors <- c("blue", "red", "orange", "purple")
    # } else if (n.tickers > 4) {
    #   #colors <- distinctColorPalette(n.tickers)
    #   colors <- colorRampPalette(c("blue", "red", "darkgreen"))(n.tickers)
    # }
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- list_override(list1 = list(x = x,
                                          y = y, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list_override(list1 = list(x = x, y = y,
                                            col = colors,
                                            cex = 0.8, pch = 16),
                               list2 = points.list)
  if (is.null(tickerlabel.offsets)) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = rep(0, n.tickers),
                                          y.offset = rep((y2 - y1) / 30,
                                                         n.tickers),
                                          stringsAsFactors = FALSE)
  } else if (is.vector(tickerlabel.offsets) &
             length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = rep(tickerlabel.offsets[1],
                                                         n.tickers),
                                          y.offset = rep(tickerlabel.offsets[2],
                                                         n.tickers),
                                          stringsAsFactors = FALSE)
  } else if (is.matrix(tickerlabel.offsets)) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = tickerlabel.offsets[, 1],
                                          y.offset = tickerlabel.offsets[, 2],
                                          stringsAsFactors = FALSE)
  } else if (is.data.frame(tickerlabel.offsets) &
             nrow(tickerlabel.offsets) < n.tickers) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = rep(0, n.tickers),
                                          y.offset = rep((y2 - y1) / 30,
                                                         n.tickers),
                                          stringsAsFactors = FALSE)
    for (ii in 1: nrow(tickerlabel.offsets)) {
      loc <- which(tickerlabel.offsets.dat[, 1] == tickerlabel.offsets[ii, 1])
      tickerlabel.offsets.dat[loc, 2: 3] <- tickerlabel.offsets.dat[loc, 2: 3] +
        tickerlabel.offsets[ii, 2: 3]
    }
  }
  text.list <- list_override(list1 = list(x = x + tickerlabel.offsets.dat[, 2],
                                          y = y + tickerlabel.offsets.dat[, 3],
                                          labels = tickers,
                                          col = colors, cex = 0.7),
                             list2 = text.list)
  
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
  if (y.metric %in% c("mean", "sd", "growth", "cagr", "mdd", "sharpe", 
                      "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", 
                      "pearson2", "spearman", "spearman2", "auto.pearson", 
                      "auto.spearman")) {
    abline(h = 0, lty = 2)
  } else if (y.metric %in% c("r.squared", "r.squared2")) {
    abline(h = 1, lty = 2)
  }
  if (x.metric %in% c("mean", "sd", "growth", "cagr", "mdd", "sharpe", 
                      "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", 
                      "pearson2", "spearman", "spearman2", "auto.pearson", 
                      "auto.spearman")) {
    abline(v = 0, lty = 2)
  } else if (x.metric %in% c("r.squared", "r.squared2")) {
    abline(v = 1, lty = 2)
  }
  
  # Add points
  do.call(points, points.list)
  
  # Add fund labels
  do.call(text, text.list)
  
  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }
  
  # Return data frame containing tickers and metrics
  return(data.frame(ticker = tickers,
                    x.metric = x,
                    y.metric = y,
                    row.names = NULL, stringsAsFactors = FALSE))
  
}
