#' Calculate Performance Metrics for Any Combination of Individual Funds,
#' 2-Fund Portfolios, and 3-Fund Portfolios
#'
#' Integrates \code{calc_metrics}, \code{calc_metrics_2funds}, and
#' \code{calc_metrics_3funds} into a single function, so you can compare
#' strategies of varying complexities.
#'
#' @param gains Data frame with a date variable named Date and one column of
#' gains for each investment.
#' @param metrics Character vector specifying metrics to calculate. See
#' \code{?calc_metrics} for choices.
#' @param tickers Character vector of ticker symbols, where the first three are
#' are a 3-fund set, the next three are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param step.along Numeric value controlling allocation increments along each
#' curve. Only applies to 2- and 3-fund sets.
#' @param step.between Numeric value controlling allocation increments between
#' each curve. Only applies to 3-fund sets.
#' @param prices Data frame with a date variable named Date and one column of
#' prices for each investment.
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
#' # Calculate CAGR and max drawdown for BRK-B, SPY/TLT, and VWEHX/VBLTX/VFINX
#' df <- calc_metrics_123(tickers = list("BRK-B", c("SPY", "TLT"), c("VWEHX", "VBLTX", "VFINX")))
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
                             step.along = 2.5,
                             step.between = 2.5,
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
        `Allocation 1 (%)` = 100, `Allocation 2 (%)` = NA, `Allocation 3 (%)` = NA,
        Label = paste("100%", x)
      )

      # Have to loop through all metrics here!
      df.x[[ylabel]] <- calc_metric(gains = gains.x, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
      df.x[[xlabel]] <- calc_metric(gains = gains.x, metric = x.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
      df.x$tooltip <- paste(df.x$Set,
                            "<br>", metric.info$title[y.metric], ": ", formatC(df.x[[ylabel]], metric.info$decimals[y.metric], format = "f"), metric.info$units[y.metric],
                            "<br>", metric.info$title[x.metric], ": ", formatC(df.x[[xlabel]], metric.info$decimals[x.metric], format = "f"), metric.info$units[x.metric], sep = "")
      return(df.x)

    }

    if (length(x) == 2) {

      gains.x <- as.matrix(gains[x])
      weights <- rbind(seq(0, 100, step.along), seq(100, 0, -step.along))
      c1 <- weights[1, ]
      c2 <- weights[2, ]

      wgains <- gains.x %*% weights / 100
      df.x <- tibble(
        Set = rep(paste(x, collapse = "-"), ncol(wgains)),
        Funds = 2,
        `Allocation 1 (%)` = c1, `Allocation 2 (%)` = c2, `Allocation 3 (%)` = NA,
        Label = ifelse(c1 == 100, paste("100%", x[1]), ifelse(c2 == 100, paste("100%", x[2]), NA_character_))
      )
      df.x[[ylabel]] <- apply(wgains, 2, function(y) {
        calc_metric(gains = y, metric = y.metric, units.year = units.year, benchmark.gains = benchmark.gains)
      })
      df.x[[xlabel]] <- apply(wgains, 2, function(y) {
        calc_metric(gains = y, metric = x.metric, units.year = units.year, benchmark.gains = benchmark.gains)
      })
      df.x$tooltip <- paste(c1, "% ", x[1], ", ", c2, "% ", x[2],
                            "<br>", metric.info$title[y.metric], ": ", formatC(df.x[[ylabel]], metric.info$decimals[y.metric], format = "f"), metric.info$units[y.metric],
                            "<br>", metric.info$title[x.metric], ": ", formatC(df.x[[xlabel]], metric.info$decimals[x.metric], format = "f"), metric.info$units[x.metric], sep = "")
      return(df.x)

    }

    gains.x <- as.matrix(gains[x])
    weights <- do.call(cbind, sapply(seq(0, 100, step.between), function(c1) {
      c2 <- seq(0, 100 - c1, step.along)
      rbind(c1, c2, 100 - c1 - c2)
    }))
    c1 <- weights[1, ]
    c2 <- weights[2, ]
    c3 <- weights[3, ]

    wgains <- gains.x %*% weights / 100
    df.x <- tibble(
      Set = rep(paste(x, collapse = "-"), ncol(wgains)),
      Funds = 3,
      `Allocation 1 (%)` = c1, `Allocation 2 (%)` = c2, `Allocation 3 (%)` = c3,
      Label = ifelse(c1 == 100, paste("100%", x[1]),
                     ifelse(c2 == 100, paste("100%", x[2]),
                            ifelse(c3 == 100, paste("100%", x[3]), NA_character_)))
    )
    df.x[[ylabel]] <- apply(wgains, 2, function(y) {
      calc_metric(gains = y, metric = y.metric, units.year = units.year, benchmark.gains = benchmark.gains)
    })
    df.x[[xlabel]] <- apply(wgains, 2, function(y) {
      calc_metric(gains = y, metric = x.metric, units.year = units.year, benchmark.gains = benchmark.gains)
    })
    df.x$tooltip <- paste(c1, "% ", x[1], ", ", c2, "% ", x[2], ", ", c3, "% ", x[3],
                         "<br>", metric.info$title[y.metric], ": ", formatC(df.x[[ylabel]], metric.info$decimals[y.metric], format = "f"), metric.info$units[y.metric],
                         "<br>", metric.info$title[x.metric], ": ", formatC(df.x[[xlabel]], metric.info$decimals[x.metric], format = "f"), metric.info$units[x.metric], sep = "")

    return(df.x)

  }))

  as.data.frame(df)

}
