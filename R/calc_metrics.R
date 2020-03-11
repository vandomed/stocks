#' Calculate Performance Metrics
#'
#' Useful for comparing funds on one or more metrics.
#'
#'
#' @param gains Data frame with one column of gains for each investment and a
#' date variable named Date.
#' @param metrics Character vector specifying metrics to calculate. Choices are
#' \code{"mean"}, \code{"sd"}, \code{"growth.x"} for growth of $x where x is the
#' initial value, \code{"growth"} for percent growth, \code{"cagr"} for compound
#' annualized growth rate, \code{"mdd"} for max drawdown, \code{"sharpe"},
#' \code{"sortino"}, \code{"alpha"}, \code{"alpha.annualized"}, \code{"beta"},
#' \code{"r.squared"}, \code{"pearson"} or \code{"spearman"} for
#' Pearson/Spearman correlation with benchmark, and \code{"auto.pearson"} or
#' \code{"auto.spearman"} for Pearson/Spearman autocorrelation.
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
#' # Calculate performance metrics for FANG stocks since the beginning of 2019
#' calc_metrics(tickers = fang, from = "2019-01-01")
#'
#' # Repeat, but use step-by-step approach with piping (need SPY to calculate
#' # alpha and beta)
#' c("SPY", fang) %>%
#'   load_gains(from = "2019-01-01") %>%
#'   calc_metrics()
#' }
#'
#'
#' @export
calc_metrics <- function(gains = NULL,
                         metrics = c("mean", "sd", "growth.10k", "growth", "cagr", "mdd", "sharpe",
                                     "sortino", "alpha", "alpha.annualized", "beta", "r.squared",
                                     "pearson", "spearman", "auto.pearson", "auto.spearman"),
                         prices = NULL,
                         tickers = NULL, ...,
                         benchmark = "SPY") {

  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% metrics)) {
    benchmark <- NULL
  }

  # Check that requested metrics are valid
  invalid.requests <- metrics[! (metrics %in% metric.choices | grepl("growth.", metrics, fixed = TRUE))]
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

      gains <- load_gains(tickers = unique(c(benchmark, tickers)), ...)
      if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", setdiff(benchmark, tickers)))

    } else {

      stop("You must specify 'tickers', 'gains', or 'prices'")

    }

  } else {
    if (is.null(tickers)) tickers <- setdiff(names(gains), "Date")
  }

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
  names(df) <- c("Fund", stocks:::metric_label(metrics))
  rownames(df) <- NULL

  return(df)

}
