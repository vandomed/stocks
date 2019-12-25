#' Calculate Performance Metrics over Time
#'
#' Useful for assessing how one or two performance metrics vary over time, for
#' one or several funds. Supports fixed-width rolling windows, fixed-width
#' disjoint windows, and disjoint windows on per-month or per-year basis.
#'
#'
#' @param gains Data frame with one column of gains for each investment and a
#' date variable named Date.
#' @param metrics Character vector specifying metrics to calculate. See
#' \code{?calc_metrics} for choices.
#' @param tickers Character vector of ticker symbols that Yahoo! Finance
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param type Character string or vector specifying type of calculation.
#' Choices are (1) \code{"roll.n"} where n is a positive integer; (2)
#' \code{"hop.n"} where n is a positive integer; (3) \code{"hop.month"}; (4)
#' \code{"hop.year"}; and (5) vector of break-point dates, e.g.
#' \code{c("2019-01-01", "2019-06-01")} for 3 periods. The "roll" and "hop"
#' options correspond to rolling and disjoint windows, respectively.
#' @param minimum.n Integer value specifying the minimum number of observations
#' per period, e.g. if you want to exclude short partial months at the beginning
#' or end of the analysis period.
#' @param prices Data frame with a date variable named Date and one column of
#' prices for each investment.
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
#' # Calculate annual CAGR's, MDD's, and Sharpe ratios for FANG stocks
#' calc_metrics_overtime(
#'   tickers = c("FB", "AAPL", "NFLX", "GOOG"),
#'   metrics = c("cagr", "mdd", "sharpe"),
#'   type = "hop.year"
#' )
#' }
#'
#'
#' @export
# gains = NULL
# metrics = c("mean", "sd")
# tickers = c("VFINX", "VWEHX")
# type = "2017-03-13"
# minimum.n = 3
# prices = NULL
# benchmark = "SPY"
calc_metrics_overtime <- function(gains = NULL,
                                  metrics = c("mean", "sd"),
                                  tickers = NULL, ...,
                                  type = "hop.year",
                                  minimum.n = 3,
                                  prices = NULL,
                                  benchmark = "SPY") {

  # Check that requested metrics are valid
  invalid.requests <- metrics[! (metrics %in% metric.choices | grepl("growth.", metrics, fixed = TRUE))]
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Align benchmarks with metrics
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% metrics)) {
    benchmark <- NULL
  }
  if (is.null(benchmark)) {
    metrics <- setdiff(metrics, c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman"))
  }

  # Determine gains based on user inputs
  if (is.null(gains)) {

    if (! is.null(prices)) {

      date.var <- names(prices) == "Date"
      gains <- cbind(prices[-1, date.var, drop = FALSE],
                     sapply(prices[! date.var], pchanges))

    } else if (! is.null(tickers)) {

      gains <- load_gains(tickers = unique(c(benchmark, tickers)),
                          mutual.start = TRUE, mutual.end = TRUE, ...)
      tickers <- setdiff(names(gains), c("Date", benchmark))

    } else {
      stop("You must specify 'tickers', 'gains', or 'prices'")
    }

  } else {
    if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", benchmark))
  }

  # Drop NA's and convert to data.table
  gains <- as.data.table(gains[complete.cases(gains), , drop = FALSE])

  # Figure out conversion factor in case CAGR or annualized alpha is requested
  if (any(c("alpha.annualized", "cagr") %in% metrics)) {
    min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
    units.year <- ifelse(min.diffdates == 1, 252, ifelse(min.diffdates <= 30, 12, 1))
  }

  # Convert gains to long format
  gains.long <- merge(
    gains[, c("Date", benchmark), with = FALSE],
    gains %>%
      melt(measure.vars = tickers, variable.name = "Fund", value.name = "Gain")
  )

  # Calculate metrics
  if (substr(type[1], 1, 3) == "hop") {

    if (type == "hop.year") {
      gains.long$Period <- year(gains.long$Date)
    } else if (type == "hop.month") {
      gains.long$Period <- paste(year(gains.long$Date), month(gains.long$Date, label = TRUE), sep = "-")
    } else {
      width <- as.numeric(substr(type, 5, 10))
      gains.long$Period <- rep(rep(1: ceiling(nrow(gains) / width), each = width)[1: nrow(gains)], length(tickers))
    }

    # Drop periods with too few observations and add end date for each period
    gains.long <- gains.long[, if (.N >= minimum.n) .SD, by = .(Fund, Period)]

    df <- cbind(
      gains.long[, .(`Start Date` = first(Date), `End Date` = last(Date)), by = .(Fund, Period)],
      sapply(metrics, function(x) {
        gains.long[, calc_metric(Gain, x, units.year, get(benchmark)), by = .(Fund, Period)][[3]]
      })
    )
    names(df) <- c("Fund", "Period", "Start date", "End date", metric.info$label[metrics])

  } else if (substr(type[1], 1, 4) == "roll") {

    width <- as.numeric(substr(type, 6, 11))

    df <- cbind(
      gains.long[, .(`Start date` = Date[1: (length(Date) - width + 1)], `End date` = Date[width: length(Date)]), by = Fund],
      sapply(metrics, function(x) {
        gains.long[, rolling_metric(
          gains = Gain, metric = x, width = width, units.year = units.year,
          benchmark.gains = get(benchmark)
        ), by = Fund][[2]]
      })
    )
    names(df) <- c("Fund", "Start date", "End date", metric.info$label[metrics])

  } else {

    type <- as.Date(type)
    if (any(is.na(type))) {
      stop("The input 'type' must be one of the following: 'roll.n' where n is a positive integer, 'hop.n' where n is a positive integer, 'hop.month', 'hop.year', or a vector of date break-points.")
    }

    daterange <- range(gains.long$Date)
    breaks <- c(daterange[1], type, daterange[2])
    labels <- sapply(1: (length(breaks) - 1), function(x) {
      paste(format(breaks[x], "%m/%d/%y"), "-", format(breaks[x + 1], "%m/%d/%y"), sep = "")
    })
    gains.long$Period <- cut(gains.long$Date, breaks = breaks, labels = labels,
                             include.lowest = TRUE, right = TRUE)

    # Drop periods with too few observations and add end date for each period
    gains.long <- gains.long[, if (.N >= minimum.n) .SD, by = .(Fund, Period)]

    df <- cbind(
      gains.long[, .(`Start date` = first(Date), `End date` = last(Date)), by = .(Fund, Period)],
      sapply(metrics, function(x) {
        gains.long[, calc_metric(Gain, x, units.year, get(benchmark)), by = .(Fund, Period)][[3]]
      })
    )
    names(df) <- c("Fund", "Period", "Start date", "End date", metric.info$label[metrics])

  }

  return(as.data.frame(df))

}
