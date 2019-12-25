#' Calculate Performance Metrics for 3-Fund Portfolios with Varying Allocations
#'
#' Useful for assessing the characteristics of 3-fund portfolios.
#'
#'
#' @param gains Data frame with a date variable named Date and one column of
#' gains for each fund.
#' @param metrics Character vector specifying metrics to calculate. See
#' \code{?calc_metrics} for choices.
#' @param tickers Character vector of ticker symbols, where the first three are
#' are a 3-fund set, the next three are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param step Numeric value specifying fund allocation increments.
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
#' # Calculate CAGR and max drawdown for UPRO/VBLTX/VWEHX
#' df <- calc_metrics_3funds(metrics = c("cagr", "mdd"), tickers = c("UPRO", "VBLTX", "VWEHX"))
#' head(df)
#'
#' # To plot, just pipe into plot_metrics_3funds
#' df %>%
#'   plot_metrics_3funds()
#'
#' # Or bypass calc_metrics_3funds altogether
#' plot_metrics_3funds(formula = cagr ~ mdd, tickers = c("UPRO", "VBLTX", "VWEHX"))
#' }
#'
#'
#' @export
calc_metrics_3funds <- function(gains = NULL,
                                metrics = c("mean", "sd"),
                                tickers = NULL, ...,
                                step = 1,
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

  # Calculate metrics for each trio
  weights <- sapply(seq(0, 100, step), function(c1) {
    c2 <- unique(c(seq(0, 100 - c1, step), 100 - c1))
    rbind(c1, c2, 100 - c1 - c2)
  })
  weights <- do.call(cbind, weights)
  w1 <- weights[1, ]
  w2 <- weights[2, ]
  w3 <- weights[3, ]

  df <- bind_rows(lapply(seq(1, length(tickers), 3), function(x) {
    gains.trio <- as.matrix(gains[tickers[x: (x + 2)]])
    wgains.trio <- gains.trio %*% weights / 100
    df.trio <- tibble(
      Trio = paste(colnames(gains.trio), collapse = "-"),
      `Fund 1` = colnames(gains.trio)[1],
      `Fund 2` = colnames(gains.trio)[2],
      `Fund 3` = colnames(gains.trio)[3],
      `Allocation 1 (%)` = w1,
      `Allocation 2 (%)` = w2,
      `Allocation 3 (%)` = w3,
      `Allocation (%)` = `Allocation 1 (%)`
    )
    for (y in metrics) {

      df.trio[[metric.info$label[y]]] <- apply(wgains.trio, 2, function(z) {
        calc_metric(gains = z, metric = y, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    return(df.trio)
  }))

  # Extract metrics for 100% each ticker
  df$Label <- ifelse(
    df$`Allocation 1 (%)` == 100, paste("100%", df$`Fund 1`),
    ifelse(df$`Allocation 2 (%)` == 100, paste("100%", df$`Fund 2`),
           ifelse(df$`Allocation 3 (%)` == 100, paste("100%", df$`Fund 3`), NA))
  )

  # Calculate metrics for reference funds
  if (! is.null(ref.tickers)) {

    df.ref <- tibble(Trio = ref.tickers, Label = ref.tickers, `Allocation (%)` = 50.1)
    for (x in metrics) {
      df.ref[[x]] <- sapply(gains[ref.tickers], function(y) {
        calc_metric(gains = y, metric = x, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    df <- bind_rows(df.ref, df)

  }

  as.data.frame(df)

}
