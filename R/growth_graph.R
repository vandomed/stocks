#' Graph Investment Growth
#' 
#' Useful for comparing performance of investments over time.
#' 
#' 
#' @inheritParams metrics
#' @inheritParams load_prices
#' @inheritParams twofunds_graph
#' 
#' @param initial Numeric value specifying what value to scale initial prices 
#' to. Can also be character string ending in "k", e.g. \code{"10k"} to graph 
#' growth of $10k without all the 0's.
#' @param grid.list List of arguments to pass to \code{\link[graphics]{grid}}.
#' @param legend.list List of arguments to pass to 
#' \code{\link[graphics]{legend}}.
#' 
#' 
#' @return
#' In addition to the graph, a list containing: 
#' \enumerate{
#' \item Numeric matrix named \code{prices} with prices for each investment.
#' \item Numeric vector named \code{means} with mean of gains for each 
#' investment.
#' \item Numeric matrix named \code{corr.matrix} with correlation matrix for 
#' gains for each investment.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot growth of $10k in VFINX and BRK-B
#' fig <- growth_graph(c("VFINX", "BRK-B"))
#' }
#'
#' @export
growth_graph <- function(tickers = NULL, ...,
                         gains = NULL, 
                         prices = NULL,
                         initial = "10k",
                         add.plot = FALSE,
                         colors = NULL,
                         lty = NULL,
                         plot.list = NULL,
                         points.list = NULL,
                         grid.list = NULL,
                         legend.list = NULL,
                         pdf.list = NULL,
                         bmp.list = NULL,
                         jpeg.list = NULL,
                         png.list = NULL,
                         tiff.list = NULL) {
  
  # If initial ends with "k", extract number before it and prep labels
  initial.text <- initial
  if (length(grep("k$", x = initial)) == 1) {
    initial <- as.numeric(sub("k", replacement = "", x = initial))
    ylab.text <- "Balance ($1,000)"
  } else {
    ylab.text <- "Balance ($)"
  }
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Obtain matrix of prices for each fund
    prices <- load_prices(tickers = tickers, initial = initial, ...)
    
  } else if (! is.null(gains)) {
    
    # Create matrix of prices for each fund
    prices <- gains_prices(gains = gains, initial = initial)
    
  } else if (! is.null(prices)) {
    
    # If different starting values, reset to initial
    if (length(unique(prices[1, ])) > 1) {
      for (ii in 1: ncol(prices)) {
        prices[, ii] <- prices[, ii] * initial / prices[1, ii]
      }
    }
    
  }
  
  # Set tickers to column names of prices matrix; if NULL, use Fund 1, Fund 2,
  # ...
  tickers <- colnames(prices)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(prices))
  }
  
  # Get dates
  rows <- rownames(prices)
  if (! is.null(rows)) {
    dates <- as.Date(rows)
  } else {
    dates <- 1: nrow(prices)
  }
  
  # Create color scheme and line types for plot
  n.tickers <- length(tickers)
  if (is.null(colors)) {
    if (n.tickers == 1) {
      colors <- "black"
    } else if (n.tickers == 2) {
      colors <- c("black", "red")
    } else if (n.tickers == 3) {
      colors <- c("black", "red", "blue")
    } else if (n.tickers == 4) {
      colors <- c("black", "red", "blue", "orange")
    } else if (n.tickers == 5) {
      colors <- c("black", "red", "blue", "orange", "purple")
    } else if (n.tickers > 5) {
      colors <- colorRampPalette(c("blue", "red"))(n.tickers)
    }
  }
  if (is.null(lty)) {
    lty <- rep(1, n.tickers)
  }
  
  # Figure out features of graph, based on user inputs where available
  plot.list <- 
    list_override(list1 = list(x = dates, y = prices[, 1], 
                               type = "n",
                               main = paste("Growth of $", initial.text, 
                                            sep = ""),
                               cex.main = 1.25,
                               xlab = "Date", ylab = ylab.text,
                               xlim = range(dates),
                               ylim = c(0, max(prices) * 1.05)),
                             list2 = plot.list)
  legend.list <- list_override(list1 = list(x = "topleft", col = colors,
                                            lty = lty, legend = tickers),
                               list2 = legend.list)
  grid.list <- list_override(list1 = list(nx = 0, ny = NULL),
                             list2 = grid.list)
  
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
  
  # Add lines for each fund
  for (ii in 1: ncol(prices)) {
    do.call(points, c(list(x = dates, y = prices[, ii], type = "l",
                           col = colors[ii], lty = lty[ii]), points.list))
  }
  
  # Add grid lines
  do.call(grid, grid.list)
  
  # Add legend
  do.call(legend, legend.list)
  
  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }
  
  # Return prices matrix, mean of gains for each fund, and correlation matrix
  gains <- apply(prices, 2, pchanges) * 100
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(prices = prices, means = means, corr.matrix = corr.matrix)
  return(return.list)
  
}
