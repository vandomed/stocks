#' Calculate Performance Metrics for 2-Fund Portfolios with Varying
#' Allocations
#'
#' Useful for assessing the characteristics of 2-fund portfolios.
#'
#'
#' @param gains Data frame with a date variable named Date and one column of
#' gains for each fund.
#' @param metrics Character vector specifying metrics to calculate. See
#' \code{?calc_metrics} for choices.
#' @param tickers Character vector of ticker symbols, where the first two are
#' are a 2-fund pair, the next two are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param prices Data frame with a date variable named Date and one column of
#' prices for each fund.
#' @param benchmark Character string specifying which fund to use as a
#' benchmark for metrics that require one.
#' @param ref.tickers Character vector of ticker symbols to include.
#'
#'
#' @return
#' Data frame with performance metrics for each portfolio at each allocation.
#'
#'
#' @examples
#' \dontrun{
#' # Calculate CAGR and max drawdown for UPRO/VBLTX
#' df <- calc_metrics_2funds(
#'   metrics = c("cagr", "mdd"),
#'   tickers = c("UPRO", "VBLTX")
#' )
#' head(df)
#'
#' # To plot, just pipe into plot_metrics_2funds
#' df %>%
#'   plot_metrics_2funds()
#'
#' # Or bypass calc_metrics_2funds altogether
#' plot_metrics_2funds(
#'   formula = cagr ~ mdd,
#'   tickers = c("UPRO", "VBLTX")
#' )
#' }
#'
#'
#' @export
calc_metrics_2funds <- function(gains = NULL,
                                metrics = c("mean", "sd"),
                                tickers = NULL, ...,
                                prices = NULL,
                                benchmark = "SPY",
                                ref.tickers = NULL) {

  # Check that requested metrics are valid
  invalid.requests <- setdiff(metrics, names(metric.info$label))
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% metrics)) {
    benchmark <- NULL
  }

  # Drop reference tickers that also appear in tickers
  ref.tickers <- setdiff(ref.tickers, tickers)
  if (length(ref.tickers) == 0) ref.tickers <- NULL

  # Determine gains if not pre-specified
  if (is.null(gains)) {

    if (! is.null(prices)) {

      date.var <- names(prices) == "Date"
      gains <- cbind(prices[-1, date.var, drop = FALSE],
                     sapply(prices[! date.var], pchanges))

    } else if (! is.null(tickers)) {

      gains <- load_gains(tickers = unique(c(benchmark, ref.tickers, tickers)),
                          mutual.start = TRUE, mutual.end = TRUE, ...)

    } else {

      stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")

    }

  }

  # If tickers is NULL, set to all funds other than benchmark/reference tickers
  if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", benchmark, ref.tickers))

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

  # Calculate metrics for each pair
  weights <- rbind(seq(0, 1, 0.01), seq(1, 0, -0.01))
  w1 <- weights[1, ] * 100
  w2 <- weights[2, ] * 100

  df <- lapply(seq(1, length(tickers), 2), function(x) {
    gains.pair <- as.matrix(gains[tickers[x: (x + 1)]])
    wgains.pair <- gains.pair %*% weights
    df.pair <- tibble(
      Pair = paste(colnames(gains.pair), collapse = "-"),
      `Fund 1` = colnames(gains.pair)[1],
      `Fund 2` = colnames(gains.pair)[2],
      `Allocation 1 (%)` = w1,
      `Allocation 2 (%)` = w2,
      `Allocation (%)` = `Allocation 1 (%)`
    )
    for (x in metrics) {
      df.pair[[metric.info$label[x]]] <- apply(wgains.pair, 2, function(y) {
        calc_metric(gains = y, metric = x, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    return(df.pair)
  })
  df <- bind_rows(df)

  # Extract metrics for 100% each ticker
  df$Label <- ifelse(df$`Allocation 1 (%)` == 0, paste("100%", df$`Fund 2`),
                     ifelse(df$`Allocation 2 (%)` == 0, paste("100%", df$`Fund 1`), NA))

  # Calculate metrics for reference funds
  if (! is.null(ref.tickers)) {

    df.ref <- tibble(Pair = ref.tickers, Label = ref.tickers)
    for (x in metrics) {
      df.ref[[metric.info$label[x]]] <- sapply(gains[ref.tickers], function(y) {
        calc_metric(gains = y, metric = x, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    df <- bind_rows(df.ref, df)

  }
  as.data.frame(bind_rows(df))

}
