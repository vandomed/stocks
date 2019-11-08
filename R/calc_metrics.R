#' Calculate Performance Metrics
#'
#' Useful for comparing funds on one or more metrics.
#'
#'
#' @param gains Data frame with one column of gains for each investment and a
#' date variable named Date.
#' @param metrics Character vector specifying metrics to calculate.
#' @param tickers Character vector of ticker symbols that Yahoo! Finance
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param prices Data frame with one column of prices for each investment and a
#' date variable named Date.
#' @param benchmark Character string specifying which fund to use as a
#' benchmark for metrics that require one.
#'
#'
#' @return
#' Data frame with performance metrics for each investment.
#'
#'
#' @examples
#' \dontrun{
#' # Calculate performance metrics for FANG stocks
#' calc_metrics(tickers = c("FB", "AAPL", "NFLX", "GOOG"))
#' }
#'
#'
#' @export
calc_metrics <- function(gains = NULL,
                         metrics = c("mean", "sd", "growth", "cagr", "mdd",
                                     "sharpe", "sortino", "alpha",
                                     "alpha.annualized", "beta", "r.squared",
                                     "pearson", "spearman", "auto.pearson",
                                     "auto.spearman"),
                         prices = NULL,
                         tickers = NULL, ...,
                         benchmark = "SPY") {

  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% metrics)) {
    benchmark <- NULL
  }

  # Check that requested metrics are valid
  invalid.requests <- setdiff(
    metrics,
    setdiff(names(metric.info[[1]]), "allocation")
  )
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Determine gains based on user inputs
  if (is.null(gains)) {

    if (! is.null(prices)) {

      date.var <- names(prices) == "Date"
      gains <- cbind(prices[-1, date.var, drop = FALSE],
                     sapply(prices[! date.var], pchanges))
      if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", setdiff(benchmark, tickers)))


    } else if (! is.null(tickers)) {

      gains <- load_gains(tickers = unique(c(benchmark, tickers)),
                          mutual.start = TRUE, mutual.end = TRUE, ...)
      if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", setdiff(benchmark, tickers)))

    } else {

      stop("You must specify 'tickers', 'gains', or 'prices'")

    }

  } else {
    if (is.null(tickers)) tickers <- setdiff(names(gains), "Date")
  }

  # Drop NA's
  gains <- gains[complete.cases(gains), , drop = FALSE]

  # Figure out conversion factor in case CAGR or annualized alpha is requested
  min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
  units.year <- ifelse(min.diffdates == 1, 252, ifelse(min.diffdates <= 30, 12, 1))

  # Extract benchmark gains
  if (! is.null(benchmark)) {
    benchmark.gains <- gains[[benchmark]]
  } else {
    benchmark.gains <- NULL
    metrics <- setdiff(metrics, c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman"))
  }

  # Calculate metrics
  df <- cbind(data.frame(Fund = tickers), sapply(metrics, function(x) {
    sapply(gains[tickers], function(y) {
      calc_metric(gains = y, metric = x, units.year = units.year, benchmark.gains = benchmark.gains)
    })
  }))
  names(df) <- c("Fund", metric.info$label[metrics])
  rownames(df) <- NULL

  return(df)

}
