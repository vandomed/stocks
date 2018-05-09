#' Graph Performance Metric for Various Investments
#' 
#' Useful for visualizing the performance of a group of investments. The first 
#' investment is used as the benchmark if the requested metric requires one.
#' 
#' 
#' @inheritParams metrics
#' @inheritParams twofunds_graph
#' 
#' 
#' @param y.metric Character string specifying y-axis performance metric. 
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
#' \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman 
#' autocorrelation, defined as the correlation between subsequent gains.
#' 
#' @param sort.tickers Logical value for whether to sort investments in 
#' decreasing order of the performance metric.
#' 
#' @param axis.list List of arguments to pass to \code{\link[graphics]{axis}}.
#' 
#' 
#' @return
#' In addition to the graph, a data frame containing the performance metric for 
#' each investment.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Compare annualized growth for VFINX, SSO, and UPRO
#' fig <- onemetric_graph(tickers = c("VFINX", "SSO", "UPRO"), 
#'                        plot.list = list(ylim = c(0, 50)))
#' }
#'
#' @export
onemetric_graph <- function(tickers = NULL, ...,
                            gains = NULL,
                            prices = NULL,
                            y.metric = "cagr",
                            add.plot = FALSE,
                            sort.tickers = TRUE,
                            plot.list = NULL,
                            points.list = NULL,
                            axis.list = NULL,
                            pdf.list = NULL,
                            bmp.list = NULL,
                            jpeg.list = NULL,
                            png.list = NULL,
                            tiff.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Obtain matrix of gains for each fund
    gains <- load_gains(tickers = tickers, ...)
    
  } else if (!is.null(prices)) {
    
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
  
  # Figure out how many units are in a year, for CAGR and axis labels. If
  # unknown, assume daily.
  if (hasArg(time.scale)) {
    extra.args <- list(...)
    time.scale <- extra.args$time.scale
    units.year <- ifelse(time.scale == "daily", 252,
                         ifelse(time.scale == "monthly", 12, 1))
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
  if (y.metric == "mean") {
    y <- apply(gains, 2, mean) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- apply(gains, 2, sd) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Standard deviation (%)"
  } else if (y.metric == "growth") {
    y <- apply(gains, 2, function(x) gains_rate(gains = x)) * 100
    plot.title <- "Total Growth"
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    y <- apply(gains, 2, function(x)
      gains_rate(gains = x, units.rate = units.year)) * 100
    plot.title <- "Compound Annualized Growth Rate"
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- "Maximum Drawdown"
    y.label <- "MDD (%)"
  } else if (y.metric == "sharpe") {
    y <- apply(gains, 2, function(x) sharpe(gains = x))
    plot.title <- "Sharpe Ratio"
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- apply(gains, 2, function(x) sortino(gains = x))
    plot.title <- "Sortino Ratio"
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- paste("Alpha w/ ", benchmark.ticker, sep = "")
    y.label <- "Alpha (%)"
  } else if (y.metric == "beta") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- paste("Beta w/ ", benchmark.ticker, sep = "")
    y.label <- "Beta"
  } else if (y.metric == "r.squared") {
    y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y.label <- "R-squared"
  } else if (y.metric == "pearson") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Pearson correlation"
  } else if (y.metric == "spearman") {
    y <- apply(gains, 2, function(x)
      cor(x, benchmark.gains, method = "spearman"))
    plot.title <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Spearman correlation"
  } else if (y.metric == "auto.pearson") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- "Autocorrelation"
    y.label <- paste("Pearson cor. for adjacent ", time.scale, " gains",
                     sep = "")
  } else if (y.metric == "auto.spearman") {
    y <- apply(gains, 2, function(x)
      cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- "Autocorrelation"
    y.label <- paste("Spearman cor. for adjacent ", time.scale, " gains",
                     sep = "")
  }
  
  # Sort tickers by y.metric, if requested
  if (sort.tickers) {
    order.funds <- order(y, decreasing = TRUE)
    tickers <- tickers[order.funds]
    y <- y[order.funds]
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- list_override(list1 = list(x = 1: n.tickers,
                                          y = y, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xaxt = "n",
                                          xlab = "Fund", ylab = y.label),
                             list2 = plot.list)
  points.list <- list_override(list1 = list(x = 1: n.tickers, y = y,
                                            cex = 0.8, pch = 16),
                               list2 = points.list)
  axis.list <- list_override(list1 = list(side = 1, at = 1: n.tickers,
                                          labels = tickers),
                             list2 = axis.list)
  
  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }
  
  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }
  
  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }
  
  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }
  
  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
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
                      "sortino", "alpha", "beta", "pearson", "spearman", 
                      "auto.pearson", "auto.spearman")) {
    abline(h = 0, lty = 2)
  } else if (y.metric == "r.squared") {
    abline(h = 1, lty = 2)
  }
  
  # Add points
  do.call(points, points.list)
  
  # Add fund labels
  do.call(axis, axis.list)
  
  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }
  
  # Return data frame containing tickers and metrics
  return(data.frame(ticker = tickers,
                    y.metric = y,
                    row.names = NULL, stringsAsFactors = FALSE))
  
}
