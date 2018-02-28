#' Graph One Performance Metric vs. Another for Three-Fund Portfolios as 
#' Allocation Varies
#' 
#' Useful for visualizing performance of three-fund portfolios, typically by 
#' plotting a measure of growth vs. a measure of volatility. 
#' 
#' 
#' @inheritParams twofunds_graph
#' 
#' 
#' @param tickers Character vector or matrix of ticker symbols, if you want to 
#' load data on the fly rather than specify \code{tickers.gains}. If a vector, 
#' the first through third elements are assumed to be first three-fund set, the 
#' fourth through sixth the second three-fund set, and so on. If a matrix, each 
#' three-row column should represent a different set.
#' 
#' @param intercepts Numeric vector or matrix of values to add to daily gains 
#' for each ticker. For example, if you have three tickers and want to simulate 
#' a 1\% annual expense ratio for only the third fund, you would set 
#' \code{intercepts = c(0, 0, convert.rate(-0.01, units.in = 252, units.out = 1))}. 
#' Like the \code{tickers} input, can be a vector divisible by three or a 
#' three-row matrix.
#' 
#' @param slopes Numeric vector or matrix of values to multiply daily gains for 
#' each ticker by. For example, if you have three tickers and want to simulate a 
#' 2x leveraged version of the third fund, you would set 
#' \code{slopes = c(1, 1, 2)}. The slopes are multiplied prior to adding the 
#' intercepts. Like the \code{tickers} input, can be a vector divisible by three 
#' or a three-row matrix.
#' 
#' @param tickers.gains Numeric matrix of gains, where each column has gains for 
#' a particular fund. The first through third columns are assumed to be the 
#' first three-fund set, fourth through sixth the second, and so on.
#' 
#' @param step.curves Numeric value specifying allocation increments for first 
#' fund in each set. For example, set to \code{0.2} for six curves for each 
#' three-fund set: one for 0\% fund 1, another for 20\% fund 1, and so on, up to 
#' 100\% fund 1 (which is technically a data point, not a curve).
#' 
#' 
#' @return
#' In addition to the graph, a list containing the following: 
#' \enumerate{
#' \item List named \code{portfolio.xy} where each element is a list of 
#' two-column matrices of x- and y-axis values for a particular three-fund set 
#' with a particular allocation to fund 1.
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
#' # Plot mean vs. SD for UPRO/VBLTX/VWEHX portfolio, and compare to VFINX and 
#' # BRK-B
#' fig1 <- threefunds_graph(tickers = c("VWEHX", "VBLTX", "UPRO"), 
#'                          reference.tickers = c("VFINX", "BRK-B"))
#'                       
#' # Same funds, but annualized growth vs. maximum drawdown
#' fig2 <- threefunds_graph(tickers = c("VWEHX", "VBLTX", "UPRO"), 
#'                          reference.tickers = c("VFINX", "BRK-B"),
#'                          x.metric = "mdd", y.metric = "cagr")
#'
#' @export
threefunds_graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                             ...,
                             benchmark.tickers = NULL,
                             reference.tickers = NULL,
                             tickers.gains = NULL,
                             benchmark.gains = NULL,
                             reference.gains = NULL,
                             step.data = 0.0025,
                             step.points = 0.1,
                             step.curves = 0.2,
                             x.metric = "sd",
                             y.metric = "mean",
                             tickerlabel.offsets = NULL,
                             reflabel.offsets = NULL,
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
    
    # If vectors rather than matrices are provided for tickers, intercepts, or
    # slopes, convert to 2-row matrices
    if (is.vector(tickers)) {
      tickers <- matrix(tickers, nrow = 3)
    }
    if (is.vector(intercepts)) {
      intercepts <- matrix(intercepts, nrow = 3)
    }
    if (is.vector(slopes)) {
      slopes <- matrix(slopes, nrow = 3)
    }
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- matrix(0, nrow = 3, ncol = ncol(tickers))
    }
    if (is.null(slopes)) {
      slopes <- matrix(1, nrow = 3, ncol = ncol(tickers))
    }
    
    # Create vector of "extra" tickers comprised of benchmark and reference
    # tickers
    extra.tickers <- unique(c(benchmark.tickers, reference.tickers))
    
    # Calculate gains matrix
    tickers.vec <- c(as.vector(tickers), extra.tickers)
    intercepts.vec <- c(as.vector(intercepts), rep(0, length(extra.tickers)))
    slopes.vec <- c(as.vector(slopes), rep(1, length(extra.tickers)))
    gains <- load_gains(tickers = tickers.vec, intercepts = intercepts.vec,
                        slopes = slopes.vec, ...)
    
    # Update ticker names to show intercept/slope
    tickers <- matrix(colnames(gains)[1: length(tickers)], nrow = 3)
    
    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: length(tickers), drop = F]
    extra.gains <- gains[, -c(1: length(tickers)), drop = F]
    if (! is.null(benchmark.tickers)) {
      benchmark.gains <- extra.gains[, 1: length(benchmark.tickers), drop = F]
      extra.gains <- extra.gains[, -c(1: length(benchmark.tickers)), drop = F]
    }
    if (! is.null(reference.tickers)) {
      reference.gains <- extra.gains
    }
    
  } else {
    
    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: ncol(tickers.gains), sep = "")
    }
    tickers <- matrix(tickers, nrow = 3)
    
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
  
  # Initialize portfolio.xy to store data for each three-fund set
  portfolio.xy <- list()
  
  # Loop through three-fund sets
  fund1.all <- seq(0, 1, step.curves)
  fund2.all <- seq(0, 1, step.data)
  fund3.all <- 1 - fund2.all
  num.curves <- length(fund1.all)
  num.points <- length(fund2.all)
  
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
  
  # Going in different direction. If doesn't work, revert back to version in
  # exports file
  for (ii in 1: ncol(tickers)) {
    
    # Initialize list to store x-axis values and y-axis values for current
    # three-fund set
    x <- y <- list()
    
    # Get subset of tickers.gains matrix for tickers of interest
    columns <- (ii * 3 - 2): (ii * 3)
    tickers.gains.sub <- tickers.gains[, columns]
    
    # Calculate x-axis value for each allocation
    if (x.metric == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean) * 100
      x <- lapply(fund1.all, function(x)
        x * means[1] + (1 - x) * fund2.all * means[2] +
          (1 - x) * fund3.all * means[3])
      
    } else if (x.metric == "sd") {
      
      vars <- var(tickers.gains.sub * 100)
      x <- lapply(fund1.all, function(x)
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] +
               ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] +
               2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3]))
      
    } else if (x.metric == "growth") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains_rate(gains = x) * 100)
      })
      
    } else if (x.metric == "cagr") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x)
                gains_rate(gains = x, units.rate = units.year) * 100)
      })
      
    } else if (x.metric == "mdd") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) mdd(gains = x) * 100)
      })
      
    } else if (x.metric == "sharpe") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sharpe(gains = x))
      })
      
    } else if (x.metric == "sortino") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sortino(gains = x))
      })
      
    } else if (x.metric == "alpha") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1] * 100)
      })
      
    } else if (x.metric == "alpha2") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1] * 100)
      })
      
    } else if (x.metric == "beta") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2] * 100)
      })
      
    } else if (x.metric == "beta2") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2] * 100)
      })
      
    } else if (x.metric == "r.squared") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
      })
      
    } else if (x.metric == "r.squared2") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
      })
      
    } else if (x.metric == "pearson") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1]))
      })
      
    } else if (x.metric == "pearson2") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2]))
      })
      
    } else if (x.metric == "spearman") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
      })
      
    } else if (x.metric == "spearman2") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
      })
      
    } else if (x.metric == "auto.pearson") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1]))
      })
      
    } else if (x.metric == "auto.spearman") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
      })
      
    } else if (x.metric == "allocation") {
      
      x <- lapply(fund1.all, function(x) fund2.all * 100)
      
    }
    
    # Calculate y-axis value for each allocation
    if (y.metric == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean) * 100
      y <- lapply(fund1.all, function(x)
        x * means[1] + (1 - x) * fund2.all * means[2] +
          (1 - x) * fund3.all * means[3])
      
    } else if (y.metric == "sd") {
      
      vars <- var(tickers.gains.sub * 100)
      y <- lapply(fund1.all, function(x)
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] +
               ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] +
               2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3]))
      
    } else if (y.metric == "growth") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains_rate(gains = x) * 100)
      })
      
    } else if (y.metric == "cagr") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x)
                gains_rate(gains = x, units.rate = units.year) * 100)
      })
      
    } else if (y.metric == "mdd") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) mdd(gains = x) * 100)
      })
      
    } else if (y.metric == "sharpe") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sharpe(gains = x))
      })
      
    } else if (y.metric == "sortino") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sortino(gains = x))
      })
      
    } else if (y.metric == "alpha") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1] * 100)
      })
      
    } else if (y.metric == "alpha2") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1] * 100)
      })
      
    } else if (y.metric == "beta") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2] * 100)
      })
      
    } else if (y.metric == "beta2") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2] * 100)
      })
      
    } else if (y.metric == "r.squared") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
      })
      
    } else if (y.metric == "r.squared2") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
      })
      
    } else if (y.metric == "pearson") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1]))
      })
      
    } else if (y.metric == "pearson2") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2]))
      })
      
    } else if (y.metric == "spearman") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
      })
      
    } else if (y.metric == "spearman2") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
      })
      
    } else if (y.metric == "auto.pearson") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1]))
      })
      
    } else if (y.metric == "auto.spearman") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
      })
      
    } else if (y.metric == "allocation") {
      
      y <- lapply(fund1.all, function(x) fund2.all * 100)
      
    }
    
    # Create list where each element is a two-column matrix of x and y values
    # for a particular fund-1 allocation
    set.list <- mapply(function(x, y)
      list(cbind(unlist(x), unlist(y))), x, y, SIMPLIFY = TRUE)
    names(set.list) <- paste(fund1.all * 100, "% ", tickers[1, ii], sep = "")
    
    # Add set.list to portfolio.xy
    portfolio.xy[[ii]] <- set.list
    
  }
  
  # Create labels for 3-fund sets
  set.labels <- apply(tickers, 2, function(x)
    paste(x[1], "-", x[2], "-", x[3], sep = ""))
  
  # Extract all x and y values from portfolio.xy
  x <- lapply(portfolio.xy, function(x) lapply(x, function(x) x[, 1]))
  y <- lapply(portfolio.xy, function(x) lapply(x, function(x) x[, 2]))
  
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
      reference.y <- apply(reference.gains, 2, function(x)
        mdd(gains = x)) * 100
    }
    y1 <- 0
  } else if (y.metric == "sharpe") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe(gains = x))
    }
  } else if (y.metric == "sortino") {
    plot.title <- "Sortino Ratio vs. "
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
    y.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
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
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale), " Gains",
                        sep = "")
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
      reference.x <- apply(reference.gains, 2, function(x)
        mdd(gains = x)) * 100
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
      reference.x <- apply(reference.gains, 2, function(x) sharpe(gains = x))
    }
  } else if (x.metric == "alpha") {
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 1])$coef[1]) * 100
    }
  } else if (x.metric == "alpha2") {
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 2])$coef[1]) * 100
    }
  } else if (x.metric == "beta") {
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 1])$coef[2])
    }
  } else if (x.metric == "beta2") {
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        lm(x ~ benchmark.gains[, 2])$coef[2])
    }
  } else if (x.metric == "r.squared") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 1]))
    }
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 2]))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 1], method = "spearman"))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x)
        cor(x, benchmark.gains[, 2], method = "spearman"))
    }
  } else if (x.metric == "auto.pearson") {
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
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
    plot.title <- paste(plot.title, "Allocation", sep = "")
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
  n.sets <- ncol(tickers)
  if (is.null(colors)) {
    if (n.sets == 1) {
      colors <- "black"
    } else if (n.sets == 2) {
      colors <- c("blue", "red")
    } else if (n.sets == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.sets == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.sets > 4) {
      colors <- colorRampPalette(c("blue", "red"))(n.sets)
    }
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- list_override(list1 = list(x = 0, y = 0, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list_override(list1 = list(pch = 16, cex = 0.7),
                               list2 = points.list)
  text.list <- list_override(list1 = list(cex = 0.7),
                             list2 = text.list)
  
  # Figure out positioning of ticker labels for 100% allocation to each fund
  if (is.null(tickerlabel.offsets)) {
    
    tickerlabel.offsets <- cbind(rep(0, n.sets * 3), rep(NA, n.sets * 3))
    y.offset.mag <- (y2 - y1) / 40
    for (ii in 1: ncol(tickers)) {
      
      # Put label for ticker with higher y-value above its data point, and
      # label for other ticker below its data point
      fund1.xy <- portfolio.xy[[ii]][[num.curves]][1, 1: 2]
      fund2.xy <- portfolio.xy[[ii]][[1]][num.points, 1: 2]
      fund3.xy <- portfolio.xy[[ii]][[1]][1, 1: 2]
      whichmax.y <- which.max(c(fund1.xy[2], fund2.xy[2], fund3.xy[2]))
      if (whichmax.y == 1) {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- -y.offset.mag
      } else if (whichmax.y == 2) {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- -y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- -y.offset.mag
      } else {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- -y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- y.offset.mag
      }
    }
    
  } else if (length(tickerlabel.offsets) == 3) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.sets * 3),
                                 rep(tickerlabel.offsets[2], n.sets * 3))
  }
  if (is.null(reflabel.offsets) & !is.null(reference.tickers)) {
    reflabel.offsets <- cbind(rep(0, length(reference.tickers)),
                              rep((y2 - y1) / 40, length(reference.tickers)))
  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.sets * 3),
                                 rep(tickerlabel.offsets[2], n.sets * 3))
  }
  
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
  
  # Add curves for each set
  for (ii in 1: n.sets) {
    
    # Add colored curves and data points
    lapply(portfolio.xy[[ii]], function(x) {
      do.call(points, c(list(x = x[, 1], y = x[, 2], type = "l",
                             col = colors[ii]), points.list))
      do.call(points, c(list(x = x[locs.points, 1], y = x[locs.points, 2],
                             col = colors[ii]), points.list))
    })
    
    # Figure out (x, y) coordinates for 100% fund 1, 100% fund 2, and 100% fund
    # 3
    fund1.xy <- portfolio.xy[[ii]][[num.curves]][1, 1: 2]
    fund2.xy <- portfolio.xy[[ii]][[1]][num.points, 1: 2]
    fund3.xy <- portfolio.xy[[ii]][[1]][1, 1: 2]
    
    # Add black data points at 100% fund 1, 100% fund2, and 100% fund 3
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))
    do.call(points, c(list(x = fund3.xy[1], y = fund3.xy[2]), points.list))
    
    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[1, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund1.xy[1] +
                             tickerlabel.offsets[(ii * 3 - 2), 1],
                           y = fund1.xy[2] +
                             tickerlabel.offsets[(ii * 3 - 2), 2],
                           label = paste("100% ", tickers[1, ii], sep = "")),
                      text.list))
    }
    if (ii == 1 | ! tickers[2, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund2.xy[1] +
                             tickerlabel.offsets[(ii * 3 - 1), 1],
                           y = fund2.xy[2] +
                             tickerlabel.offsets[(ii * 3 - 1), 2],
                           label = paste("100% ", tickers[2, ii], sep = "")),
                      text.list))
    }
    if (ii == 1 | ! tickers[3, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund3.xy[1] + tickerlabel.offsets[(ii * 3), 1],
                           y = fund3.xy[2] + tickerlabel.offsets[ii * 3, 2],
                           label = paste("100% ", tickers[3, ii], sep = "")),
                      text.list))
    }
    
  }
  
  # Add data point for reference funds (if given)
  if (!is.null(reference.tickers)) {
    
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
  
  # Return portfolio.xy, mean for each fund, and correlation matrix
  if (! exists("gains")) {
    gains <- cbind(tickers.gains, benchmark.gains, reference.gains)
  }
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(portfolio.xy = portfolio.xy, means = means,
                      corr.matrix = corr.matrix)
  return(return.list)
  
}
