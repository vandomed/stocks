#' Calculate Performance Metrics for Any Combination of Individual Funds,
#' 2-Fund Portfolios, and 3-Fund Portfolios
#'
#' Integrates \code{calc_metrics}, \code{calc_metrics_2funds}, and
#' \code{calc_metrics_3funds} into a single function, so you can compare
#' strategies of varying complexities.
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
#'
#'
#' @return
#' Data frame with performance metrics for each portfolio at each allocation.
#'
#'
#' @examples
#' \dontrun{
#' # Calculate CAGR vs. max drawdown for BRK-B, SPY/TLT, and VWEHX/VBLTX/VFINX
#' df <- calc_metrics_123(
#'   tickers = list("BRK-B", c("SPY", "TLT"), c("VWEHX", "VBLTX", "VFINX")),
#'   metrics = c("cagr", "mdd")
#' )
#' head(df)
#'
#' # To plot, just pipe into plot_metrics_123
#' df %>%
#'   plot_metrics_123()
#'
#' # Or bypass calc_metrics_123 altogether
#' plot_metrics_123(
#'   formula = cagr ~ mdd,
#'   tickers = list("BRK-B", c("SPY", "TLT"), c("VWEHX", "VBLTX", "VFINX"))
#' )
#' }
#'
#'
#' @export
calc_metrics_123 <- function(gains = NULL,
                             metrics = c("mean", "sd"),
                             tickers = NULL, ...,
                             step = 1,
                             prices = NULL,
                             benchmark = "SPY") {

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

  # Determine gains if not pre-specified
  if (is.null(gains)) {

    if (! is.null(prices)) {

      date.var <- names(prices) == "Date"
      gains <- cbind(prices[-1, date.var, drop = FALSE],
                     sapply(prices[! date.var], pchanges))

    } else if (! is.null(tickers)) {

      gains <- load_gains(tickers = unique(c(benchmark, unlist(tickers))),
                          mutual.start = TRUE, mutual.end = TRUE, ...)

    } else {

      stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")

    }

  }

  # If tickers is NULL, set to all single funds other than benchmarks
  if (is.null(tickers)) tickers <- as.list(setdiff(names(gains), c("Date", benchmark)))

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

  # Loop through and calculate metrics for each set
  df <- do.call(rbind, lapply(tickers, function(x) {

    if (length(x) == 1) {

      gains.x <- gains[[x]]
      df.x <- tibble(
        Set = x,
        Funds = 1,
        `Fund 1` = x, `Fund 2` = NA, `Fund 3` = NA,
        `Allocation 1 (%)` = 100, `Allocation 2 (%)` = NA, `Allocation 3 (%)` = NA,
        Label = x
      )

      for (mtrc in metrics) {
        df.x[[metric.info$label[mtrc]]] <- calc_metric(gains = gains.x, metric = mtrc, units.year = units.year, benchmark.gains = benchmark.gains)
      }
      return(df.x)

    }

    if (length(x) == 2) {

      gains.x <- as.matrix(gains[x])
      weights <- rbind(seq(0, 100, step), seq(100, 0, -step))
      c1 <- weights[1, ]
      c2 <- weights[2, ]

      wgains <- gains.x %*% weights / 100
      df.x <- tibble(
        Set = rep(paste(x, collapse = "-"), ncol(wgains)),
        Funds = 2,
        `Fund 1` = x[1], `Fund 2` = x[2], `Fund 3` = NA,
        `Allocation 1 (%)` = c1, `Allocation 2 (%)` = c2, `Allocation 3 (%)` = NA,
        Label = ifelse(c1 == 100, paste("100%", x[1]), ifelse(c2 == 100, paste("100%", x[2]), NA_character_))
      )
      for (mtrc in metrics) {
        df.x[[metric.info$label[mtrc]]] <- apply(wgains, 2, function(y) {
          calc_metric(gains = y, metric = mtrc, units.year = units.year, benchmark.gains = benchmark.gains)
        })
      }
      return(df.x)

    }

    gains.x <- as.matrix(gains[x])
    weights <- do.call(cbind, sapply(seq(0, 100, step), function(c1) {
      c2 <- seq(0, 100 - c1, step)
      rbind(c1, c2, 100 - c1 - c2)
    }))
    c1 <- weights[1, ]
    c2 <- weights[2, ]
    c3 <- weights[3, ]

    wgains <- gains.x %*% weights / 100
    df.x <- tibble(
      Set = rep(paste(x, collapse = "-"), ncol(wgains)),
      Funds = 3,
      `Fund 1` = x[1], `Fund 2` = x[2], `Fund 3` = x[3],
      `Allocation 1 (%)` = c1, `Allocation 2 (%)` = c2, `Allocation 3 (%)` = c3,
      Label = ifelse(c1 == 100, paste("100%", x[1]),
                     ifelse(c2 == 100, paste("100%", x[2]),
                            ifelse(c3 == 100, paste("100%", x[3]), NA_character_)))
    )
    for (mtrc in metrics) {
      df.x[[metric.info$label[mtrc]]] <- apply(wgains, 2, function(y) {
        calc_metric(gains = y, metric = mtrc, units.year = units.year, benchmark.gains = benchmark.gains)
      })
    }
    return(df.x)

  }))

  as.data.frame(df)

}
