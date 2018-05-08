#' Scatterplot of Investment Gains
#' 
#' Useful for visualizing relationship between one (or several) investments and 
#' a benchmark. First fund in \code{tickers}, \code{gains}, or \code{prices} is 
#' used as the benchmark.
#' 
#' 
#' @inheritParams metrics
#' @inheritParams twofunds_graph
#' 
#' @param orders Numeric vector specifying the orders of linear regression 
#' models for each y-axis investment. Set to \code{1} for simple linear 
#' regression, \code{2} for linear regression with first- and second-order 
#' terms, and so on.
#' 
#' @param include.legend Logical value.
#' 
#' @param legend.list List of arguments to pass to 
#' \code{\link[graphics]{legend}}.
#' 
#' 
#' @return
#' In addition to the graph, a list containing fitted linear regression models 
#' returned by \code{\link[stats]{lm}} for each investment vs. the benchmark.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot daily gains for SSO and UPRO vs. VFINX
#' fig <- gains_graph(c("VFINX", "SSO", "UPRO"))
#' }
#'
#' @export
gains_graph <- function(tickers = NULL, ...,
                        gains = NULL,
                        prices = NULL,
                        orders = 1,
                        add.plot = FALSE,
                        include.legend = TRUE,
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
    gains <- load_gains(tickers = tickers, ...) * 100
    
  } else if (! is.null(prices)) {
    
    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges) * 100
    rownames(gains) <- rownames(prices)[-1]
    
  } else if (is.null(gains)) {
    
    stop("You must specify 'tickers', 'gains', or 'prices'")
    
  }
  
  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2,
  # ...
  tickers <- colnames(gains)
  n.tickers <- length(tickers)
  n.curves <- n.tickers - 1
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
  
  # Create color scheme for plot
  if (is.null(colors)) {
    if (n.tickers == 2) {
      colors <- "black"
    } else if (n.tickers == 3) {
      colors <- c("black", "red")
    } else if (n.tickers == 4) {
      colors <- c("black", "red", "blue")
    } else if (n.tickers == 5) {
      colors <- c("black", "red", "blue", "orange")
    } else if (n.tickers == 6) {
      colors <- c("black", "red", "blue", "orange", "purple")
    } else if (n.tickers > 6) {
      colors <- colorRampPalette(c("blue", "red"))(n.curves)
    }
  }
  if (is.null(lty)) {
    lty <- rep(1, n.curves)
  }
  
  # Figure out features of graph, based on user inputs where available
  time.scale.cap <- capitalize(time.scale)
  plot.list <-
    list_override(list1 = list(x = 0, y = 0, type = "n",
                               main = paste("Scatterplot of", time.scale.cap,
                                            "Gains"),
                               cex.main = 1.25,
                               xlab = paste(time.scale.cap,
                                            "gains for", tickers[1], "(%)"),
                               ylab = ifelse(n.tickers == 2,
                                             paste(time.scale.cap, "gains for",
                                                   tickers[2], "(%)"),
                                             paste(time.scale.cap, "gains (%)")),
                               xlim = range(gains[, 1]) * 1.05,
                               ylim = range(gains[, -1]) * 1.05),
                  list2 = plot.list)
  points.list <- list_override(list1 = list(cex = 0.6),
                               list2 = points.list)
  
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
  
  # If orders is NULL, set to 1's; if scalar, extend to vector
  if (is.null(orders)) {
    orders <- rep(1, n.curves)
  } else if (length(orders) == 1 & n.tickers > 2) {
    orders <- rep(orders, n.curves)
  }
  
  # Add dotted lines at x = 0 and at y = 0
  abline(h = 0, lty = 2)
  abline(v = 0, lty = 2)
  
  # Add points and regression line for each fund
  lm.fits <- list()
  legend.entries <- list()
  for (ii in 1: (n.tickers - 1)) {
    do.call(points, c(list(x = gains[, 1], y = gains[, ii + 1],
                           col = colors[ii]),
                      points.list))
    if (orders[ii] == 1) {
      fit <- lm(gains[, (ii + 1)] ~ gains[, 1])
      legend.entries[[ii]] <-
        bquote(.(tickers[ii + 1]): Y ==
                 .(paste(sprintf("%.3f", fit$coef[1]),
                         ifelse(fit$coef[2] > 0, " + ", " - "),
                         sprintf("%.3f", abs(fit$coef[2])),
                         "X", sep = "")) ~
                 .("(") * R^2 == .(paste(sprintf("%.2f",
                                                 summary(fit)$r.squared),
                                         ")", sep = "")))
      
    } else {
      fit <- lm(gains[, (ii + 1)] ~ poly(gains[, 1], orders[ii], raw = TRUE))
      if (orders[ii] == 2) {
        legend.entries[[ii]] <-
          bquote(.(tickers[ii + 1]): Y ==
                   .(paste(sprintf("%.3f", fit$coef[1]),
                           ifelse(fit$coef[2] > 0, " + ", " - "),
                           sprintf("%.3f", abs(fit$coef[2])), "X",
                           ifelse(fit$coef[3] > 0, " + ", " - "),
                           sprintf("%.3f", abs(fit$coef[3])), sep = "")) * X^2 ~
                   .("(") * R^2 == .(paste(sprintf("%.2f",
                                                   summary(fit)$r.squared),
                                           ")", sep = "")))
      } else {
        legend.entries[[ii]] <- tickers[ii + 1]
      }
    }
    xy <- cbind(gains[, 1], predict(fit))
    xy <- xy[order(xy[, 1]), ]
    do.call(points, c(list(x = xy[, 1], y = xy[, 2], type = "l",
                           col = colors[ii], lty = lty[ii]),
                      points.list))
    lm.fits[[ii]] <- fit
  }
  
  # Add legend
  if (include.legend) {
    legend.list <- list_override(list1 = list(x = "topleft", lty = lty,
                                              col = colors, cex = 0.7,
                                              legend = sapply(legend.entries,
                                                              as.expression)),
                                 list2 = legend.list)
    do.call(legend, legend.list)
  }
  
  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }
  
  # Return fitted models
  return(lm.fits)
  
}
