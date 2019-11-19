#' Plot One Performance Metric over Time or One vs. Another over Time
#'
#' Useful for assessing how one or two performance metrics vary over time, for
#' one or several funds. Supports fixed-width rolling windows, fixed-width
#' disjoint windows, and disjoint windows on per-month or per-year basis.
#'
#'
#' @param metrics "Long" data frame with Fund column, Date column, and column
#' for each metric you want to plot. Typically the result of a prior call to
#' \code{\link{calc_metrics_overtime}}.
#' @param formula Formula specifying what to plot, e.g. \code{cagr ~ mdd} for
#' CAGR vs. MDD or \code{cagr ~ .} for CAGR over time. See \code{?calc_metrics}
#' for list of performance metrics to choose from.
#' @param type Character string specifying type of calculation. Choices are
#' \code{"roll.n"} where n is a positive integer, \code{"hop.n"} where n is a
#' positive integer, \code{hop.month}, and \code{hop.year}.
#' @param minimum.n Integer value specifying the minimum number of observations
#' per period, e.g. if you want to exclude short partial months at the beginning
#' or end of the analysis period.
#' @param tickers Character vector of ticker symbols that Yahoo! Finance
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param gains Data frame with a date variable named Date and one column of
#' gains for each investment.
#' @param prices Data frame with a date variable named Date and one column of
#' prices for each investment.
#' @param benchmark,y.benchmark,x.benchmark Character string specifying which
#' fund to use as benchmark for metrics (if you request \code{alpha},
#' \code{alpha.annualized}, \code{beta}, or \code{r.squared}).
#' @param plotly Logical value for whether to convert the
#' \code{\link[ggplot2]{ggplot}} to a \code{\link[plotly]{plotly}} object
#' internally.
#' @param title Character string. Only really useful if you're going to set
#' \code{plotly = TRUE}, otherwise you can change the title, axes, etc.
#' afterwards.
#' @param return Character string specifying what to return. Choices are
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#'
#'
#' @return
#' Depending on \code{return}, a \code{\link[ggplot2]{ggplot}}, a data frame
#' with the source data, or a list containing both.
#'
#'
#' @examples
#' \dontrun{
#' # Plot net growth each year for BRK-B and SPY, using approach (1)
#' plot_metrics_overtime(formula = growth ~ ., type = "hop.year", tickers = c("BRK-B", "SPY"))
#'
#' # Plot Sharpe ratios each month for FANG stocks, using approach (2)
#' gains <- load_gains(c("FB", "AAPL", "NFLX", "GOOG"))
#' plot_metrics_overtime(formula = sharpe ~ ., gains = gains)
#'
#' # Plot betas from 100-day disjoint intervals for a 2x daily (SSO) and 3x
#' # daily (UPRO) leveraged ETF, using approach (3) and piping
#' c("SPY", "SSO", "UPRO") %>%
#'   load_gains() %>%
#'   calc_metrics_overtime(metrics = "beta", type = "hop.100") %>%
#'   plot_metrics_overtime(formula = beta ~ .)
#' }
#'
#' @export
plot_metrics_overtime <- function(metrics = NULL,
                                  formula = cagr ~ .,
                                  type = "hop.year",
                                  minimum.n = 3,
                                  tickers = NULL, ...,
                                  gains = NULL,
                                  prices = NULL,
                                  benchmark = "SPY",
                                  y.benchmark = benchmark,
                                  x.benchmark = benchmark,
                                  plotly = FALSE,
                                  title = NULL,
                                  return = "plot") {

  # Extract info from formula
  all.metrics <- all.vars(formula, functions = FALSE)
  if (! is.null(metrics) & ! all(metric.info$label[all.metrics] %in% names(metrics))) {
    all.metrics <- names(metric.info$label[metric.info$label %in% intersect(names(metrics), metric.info$label)])
    if (length(all.metrics) == 1) {
      all.metrics <- c(all.metrics, ".")
    } else if (length(all.metrics) >= 2) {
      all.metrics <- all.metrics[1: 2]
    } else {
      stop("The input 'metrics' must have at least one column with a performance metric")
    }
  }
  y.metric <- x.metric <- NULL
  if (all.metrics[1] != ".") y.metric <- all.metrics[1]
  if (all.metrics[2] != ".") x.metric <- all.metrics[2]
  all.metrics <- c(x.metric, y.metric)

  ylabel <- metric.info$label[y.metric]
  xlabel <- metric.info$label[x.metric]

  # Align benchmarks with metrics
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% y.metric)) {
    y.benchmark <- NULL
  }
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% x.metric)) {
    x.benchmark <- NULL
  }

  # Check that requested metrics are valid
  invalid.requests <- setdiff(
    all.metrics,
    setdiff(names(metric.info[[1]]), "allocation")
  )
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Calculate performance metrics if not pre-specified
  if (is.null(metrics)) {

    # Download data if not pre-specified
    if (is.null(gains)) {

      if (! is.null(prices)) {

        date.var <- names(prices) == "Date"
        gains <- cbind(prices[-1, date.var, drop = FALSE],
                       sapply(prices[! date.var], pchanges))

      } else if (! is.null(tickers)) {

        gains <- load_gains(tickers = c(unique(c(y.benchmark, x.benchmark)), tickers),
                            mutual.start = TRUE, mutual.end = TRUE, ...)
        tickers <- setdiff(names(gains), c("Date", y.benchmark, x.benchmark))

      } else {
        stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")
      }

    } else {
      if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", y.benchmark, x.benchmark))
    }

    # Drop NA's and convert to data.table
    gains <- as.data.table(gains[complete.cases(gains), , drop = FALSE])

    # Figure out conversion factor in case CAGR or annualized alpha is requested
    min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
    time.units <- ifelse(min.diffdates == 1, "day", ifelse(min.diffdates <= 30, "month", "year"))
    units.year <- ifelse(time.units == "day", 252, ifelse(time.units == "month", 12, 1))

    # Convert gains to long format
    gains.long <- merge(
      gains[, c("Date", unique(c(y.benchmark, x.benchmark))), with = FALSE],
      gains %>%
      melt(measure.vars = tickers, variable.name = "Fund", value.name = "Gain"))

    # Calculate metrics depending on user choice for type
    if (substr(type, 1, 3) == "hop") {

      # Add Period variable
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
      df <- gains.long[, .(Date = last(Date)), by = .(Fund, Period)]

      if (! is.null(y.metric)) {
        df[[ylabel]] <- gains.long[, calc_metric(
          gains = Gain, metric = y.metric, units.year = units.year, benchmark.gains = get(y.benchmark)
        ), by = .(Fund, Period)][[3]]
      }

      if (! is.null(x.metric)) {
        df[[xlabel]] <- gains.long[, calc_metric(
          gains = Gain, metric = x.metric, units.year = units.year, benchmark.gains = get(x.benchmark)
        ), by = .(Fund, Period)][[3]]
      }

    } else if (substr(type, 1, 4) == "roll") {

      width <- as.numeric(substr(type, 6, 11))
      df <- gains.long[, .(Date = Date[width: length(Date)]), Fund]

      if (! is.null(y.metric)) {
        df[[ylabel]] <- gains.long[, rolling_metric(
          gains = Gain, metric = y.metric, width = width, units.year = units.year, benchmark.gains = get(y.benchmark)
        ), Fund][[2]]
      }

      if (! is.null(x.metric)) {
        df[[xlabel]] <- gains.long[, rolling_metric(
          gains = Gain, metric = x.metric, width = width, units.year = units.year, benchmark.gains = get(x.benchmark)
        ), Fund][[2]]
      }

    } else {
      stop("The input 'type' must be one of the following: 'roll.n' where n is a positive integer, 'hop.n' where n is a positive integer, 'hop.month', or 'hop.year'")
    }

  } else {
    df <- metrics
  }

  # Create plot
  df <- as.data.frame(df)
  df <- df[order(df$Fund, df$Date), c("Date", "Fund", c(ylabel, xlabel))]

  if (is.null(x.metric)) {

    df$text <- paste("Fund: ", df$Fund,
                     "<br>End date: ", df$Date,
                     "<br>", ylabel, ": ", formatC(df[[ylabel]], metric.info$decimals[y.metric], format = "f"), sep = "")
    p <- ggplot(df, aes(y = .data[[ylabel]],
                        x = Date,
                        group = Fund, color = Fund, text = text)) +
      geom_point() +
      geom_path() +
      ylim(range(c(0, df[[ylabel]])) * 1.01) +
      theme(legend.position = "none") +
      labs(title = paste(metric.info$title[y.metric], "over Time"),
           y = metric.info$label[y.metric], x = "End date")

  } else if (is.null(y.metric)) {

    df$text <- paste("Fund: ", df$Fund,
                     "<br>End date: ", df$Date,
                     "<br>", xlabel, ": ", formatC(df[[xlabel]], metric.info$decimals[x.metric], format = "f"), sep = "")
    p <- ggplot(df, aes(y = Date,
                        x = .data[[xlabel]],
                        group = Fund, color = Fund, text = text)) +
      geom_point() +
      geom_path() +
      xlim(range(c(0, df[[xlabel]])) * 1.01) +
      theme(legend.position = "none") +
      labs(title = ifelse(! is.null(title), title, paste(metric.info$title[y.metric], "over Time")),
           y = "End date", x = xlabel)

  } else {

    df$text <- paste("Fund: ", df$Fund,
                     "<br>End date: ", df$Date,
                     "<br>", xlabel, ": ", formatC(df[[xlabel]], metric.info$decimals[x.metric], format = "f"),
                     "<br>", ylabel, ": ", formatC(df[[ylabel]], metric.info$decimals[y.metric], format = "f"), sep = "")
    p <- ggplot(df, aes(y = .data[[ylabel]],
                        x = .data[[xlabel]],
                        group = Fund, color = Fund, text = text)) +
      geom_path() +
      geom_point() +
      geom_point(data = df %>% group_by(Fund) %>% slice(1) %>% ungroup(), show.legend = FALSE) +
      geom_path(data = df %>% group_by(Fund) %>% slice(-1) %>% ungroup(), show.legend = FALSE,
                arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) +
      ylim(range(c(0, df[[ylabel]])) * 1.01) +
      xlim(range(c(0, df[[xlabel]])) * 1.01) +
      theme(legend.position = "none") +
      labs(title = ifelse(! is.null(title), title, paste(metric.info$title[y.metric], "vs.", metric.info$title[x.metric])),
           y = ylabel, x = xlabel)

  }

  if (plotly) p <- ggplotly(p, tooltip = "text")
  df <- df[names(df) != "text"]

  if (return == "plot") return(p)
  if (return == "data") return(df)
  if (return == "both") return(list(plot = p, data = df))

}
