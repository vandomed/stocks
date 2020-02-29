#' Plot One Performance Metric vs. Another for 3-Fund Portfolios
#'
#' Useful for visualizing the behavior of one or several 3-fund portfolios,
#' e.g. by plotting a measure of growth vs. a measure of volatility.
#'
#'
#' @param metrics Data frame with Fund column and column for each metric you
#' want to plot. Typically the result of a prior call to
#' \code{\link{calc_metrics_3funds}}.
#' @param formula Formula specifying what to plot, e.g. \code{mean ~ sd},
#' \code{cagr ~ mdd}, or \code{sharpe ~ allocation}. See \code{?calc_metrics}
#' for list of metrics to choose from (\code{"allocation"} is an extra option
#' here). If you specify \code{metrics}, default behavior is to use
#' \code{mean ~ sd} unless either is not available, in which case the first two
#' performance metrics that appear as columns in \code{metrics} are used.
#' @param tickers Character vector of ticker symbols, where the first three are
#' are a 3-fund set, the next three are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to
#' \code{\link{load_gains}}.
#' @param step Numeric value specifying fund allocation increments.
#' @param gains Data frame with a date variable named Date and one column of
#' gains for each fund.
#' @param prices Data frame with a date variable named Date and one column of
#' prices for each fund.
#' @param benchmark,y.benchmark,x.benchmark Character string specifying which
#' fund to use as benchmark for metrics (if you request \code{alpha},
#' \code{alpha.annualized}, \code{beta}, or \code{r.squared}).
#' @param ref.tickers Character vector of ticker symbols to include on the
#' graph.
#' @param plotly Logical value for whether to convert the
#' \code{\link[ggplot2]{ggplot}} to a \code{\link[plotly]{plotly}} object
#' internally.
#' @param title Character string.
#' @param base_size Numeric value.
#' @param label_size Numeric value.
#' @param return Character string specifying what to return. Choices are
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#'
#'
#' @return
#' Depending on \code{return}, a \code{\link[ggplot2]{ggplot}} object, a data
#' frame, or a list containing both.
#'
#'
#' @examples
#' \dontrun{
#' # Plot mean vs. SD for UPRO/VBLTX/VWEHX
#' plot_metrics_3funds(
#'   formula = mean ~ sd,
#'   tickers = c("UPRO", "VBLTX", "VWEHX")
#' )
#'
#' # Plot CAGR vs. max drawdown for FB/AAPL/NFLX and SPY/TLT/JNK
#' plot_metrics_3funds(
#'   formula = cagr ~ mdd,
#'   tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK")
#' )
#'
#' # Plot Sharpe ratio vs. allocation for the same sets
#' plot_metrics_3funds(
#'   formula = sharpe ~ allocation,
#'   tickers = c("FB", "AAPL", "NFLX", "SPY", "TLT", "JNK")
#' )
#' }
#'
#'
#' @export
# metrics = NULL
# formula = mean ~ sd
# tickers = c("VFINX", "VWEHX", "VBLTX")
# step = 1
# gains = NULL
# prices = NULL
# benchmark = "SPY"
# y.benchmark = benchmark
# x.benchmark = benchmark
# ref.tickers = NULL
# plotly = FALSE
# title = NULL
# base_size = 16
# label_size = 6
# return = "plot"
plot_metrics_3funds <- function(metrics = NULL,
                                formula = mean ~ sd,
                                tickers = NULL, ...,
                                step = 1,
                                gains = NULL,
                                prices = NULL,
                                benchmark = "SPY",
                                y.benchmark = benchmark,
                                x.benchmark = benchmark,
                                ref.tickers = NULL,
                                plotly = FALSE,
                                title = NULL,
                                base_size = 16,
                                label_size = 6,
                                return = "plot") {

  # Extract info from formula
  all.metrics <- all.vars(formula, functions = FALSE)

  # If metrics is specified but doesn't include the expected variables, set defaults
  if (! is.null(metrics) & ! all(unlist(metric_label(all.metrics)) %in% names(metrics))) {
    all.metrics <- unlist(label_metric(names(metrics)))
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
  all.metrics <- c(y.metric, x.metric)

  ylabel <- metric_label(y.metric)
  xlabel <- metric_label(x.metric)

  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% all.metrics)) {
    benchmark <- y.benchmark <- x.benchmark <- NULL
  }

  # Check that requested metrics are valid
  invalid.requests <- all.metrics[! (all.metrics %in% c(metric.choices, "allocation") | grepl("growth.", all.metrics, fixed = TRUE))]
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Drop reference tickers that also appear in tickers
  ref.tickers <- setdiff(ref.tickers, tickers)
  if (length(ref.tickers) == 0) ref.tickers <- NULL

  # Calculate performance metrics if not pre-specified
  if (is.null(metrics)) {

    # Determine gains if not pre-specified
    if (is.null(gains)) {

      if (! is.null(prices)) {

        date.var <- names(prices) == "Date"
        gains <- cbind(prices[-1, date.var, drop = FALSE],
                       sapply(prices[! date.var], pchanges))

      } else if (! is.null(tickers)) {

        gains <- load_gains(tickers = unique(c(y.benchmark, x.benchmark, ref.tickers, tickers)),
                            mutual.start = TRUE, mutual.end = TRUE, ...)

      } else {

        stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")

      }

    }

    # If tickers is NULL, set to all funds other than benchmark/reference tickers
    if (is.null(tickers)) tickers <- setdiff(names(gains), c("Date", y.benchmark, x.benchmark, ref.tickers))

    # Drop NA's
    gains <- gains[complete.cases(gains), , drop = FALSE]

    # Figure out conversion factor in case CAGR or annualized alpha is requested
    min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
    units.year <- ifelse(min.diffdates == 1, 252, ifelse(min.diffdates <= 30, 12, 1))

    # Extract benchmark gains
    if (! is.null(y.benchmark)) {
      y.benchmark.gains <- gains[[y.benchmark]]
    } else {
      y.benchmark.gains <- NULL
    }
    if (! is.null(x.benchmark)) {
      x.benchmark.gains <- gains[[x.benchmark]]
    } else {
      x.benchmark.gains <- NULL
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

    df <- lapply(seq(1, length(tickers), 3), function(x) {
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
      if (y.metric != "allocation") {
        df.trio[[ylabel]] <- apply(wgains.trio, 2, function(y) {
          calc_metric(gains = y, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
        })
      }
      if (x.metric != "allocation") {
        df.trio[[xlabel]] <- apply(wgains.trio, 2, function(y) {
          calc_metric(gains = y, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
        })
      }
      return(df.trio)
    })
    df <- bind_rows(df)

  } else {
    df <- metrics
  }

  # # Jitter allocation slightly for visual purposes
  # if (x.metric == "allocation") {
  #   df <- df %>%
  #     group_by(Trio, `Allocation 1 (%)`) %>%
  #     mutate(`Allocation (%)` = `Allocation 1 (%)` + step * 0.5 * row_number() / n()) %>%
  #     ungroup()
  # }

  # Extract metrics for 100% each ticker
  df$Label <- ifelse(
    df$`Allocation 1 (%)` == 100, paste("100%", df$`Fund 1`),
    ifelse(df$`Allocation 2 (%)` == 100, paste("100%", df$`Fund 2`),
           ifelse(df$`Allocation 3 (%)` == 100, paste("100%", df$`Fund 3`), NA))
  )

  # Calculate metrics for reference funds
  if (! is.null(ref.tickers)) {

    df.ref <- tibble(Trio = ref.tickers, Label = ref.tickers)

    if (y.metric == "allocation") {
      df.ref[[ylabel]] <- 50.1
    } else {
      df.ref[[ylabel]] <- sapply(gains[ref.tickers], function(x) {
        calc_metric(gains = x, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
      })
    }

    if (x.metric == "allocation") {
      df.ref[[xlabel]] <- 50.1
    } else {
      df.ref[[xlabel]] <- sapply(gains[ref.tickers], function(x) {
        calc_metric(gains = x, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
      })
    }
    df <- bind_rows(df.ref, df)

  }

  # Prep for ggplot
  df <- as.data.frame(df)
  df$tooltip <- paste(ifelse(is.na(df$`Fund 1`), df$Trio, paste(df$`Allocation 1 (%)`, "% ", df$`Fund 1`, ", ",
                                                                df$`Allocation 2 (%)`, "% ", df$`Fund 2`, ", ",
                                                                df$`Allocation 3 (%)`, "% ", df$`Fund 3`, sep = "")),
                      "<br>", metric_title(y.metric), ": ", formatC(df[[ylabel]], metric_decimals(y.metric), format = "f"), metric_units(y.metric),
                      "<br>", metric_title(x.metric), ": ", formatC(df[[xlabel]], metric_decimals(x.metric), format = "f"), metric_units(x.metric), sep = "")
  df.points <- subset(df, Trio %in% ref.tickers | `Allocation 1 (%)` == 100 |
                        `Allocation 2 (%)` == 100 | `Allocation 3 (%)` == 100)
  if ("allocation" %in% all.metrics) {
    df.points <- subset(df.points, ! Trio %in% ref.tickers)
  }
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1: n]
  }
  cols <- c()
  trios <- setdiff(unique(df$Trio), ref.tickers)
  cols[trios] <- gg_color_hue(length(trios))
  cols[ref.tickers] <- "black"

  # Create plot
  p <- ggplot(df, aes(y = .data[[ylabel]], x = .data[[xlabel]], group = Trio, color = Trio, text = tooltip))

  if (x.metric == "allocation" & ! is.null(ref.tickers)) {
    p <- p + geom_hline(data = df.ref, yintercept = df.ref[[ylabel]], lty = 2)
  } else if (y.metric == "allocation" & ! is.null(ref.tickers)) {
    p <- p + geom_vline(data = df.ref, yintercept = df.ref[[xlabel]], lty = 2)
  }

  p <- p +
    geom_point(data = df.points, col = "black") +
    geom_path(aes(group = interaction(Trio, `Allocation 1 (%)`))) +
    geom_path(aes(group = interaction(Trio, `Allocation 2 (%)`))) +
    geom_path(data = subset(df, `Allocation 1 (%)` == 0), color = "black") +
    geom_path(data = subset(df, `Allocation 2 (%)` == 0), color = "black") +
    geom_path(data = subset(df, `Allocation 3 (%)` == 0), color = "black") +
    ylim(range(c(0, df[[ylabel]])) * 1.01) +
    xlim(range(c(0, df[[xlabel]])) * 1.01) +
    scale_colour_manual(values = cols) +
    theme_gray(base_size = base_size) +
    theme(legend.position = "none") +
    labs(title = ifelse(! is.null(title), title, paste(metric_title(y.metric), "vs.", metric_title(x.metric))),
         y = ylabel, x = xlabel)

  if (plotly) {
    p <- ggplotly(p, tooltip = "tooltip") %>%
      style(hoverlabel = list(font = list(size = 15)))
  } else {
    p <- p + geom_label_repel(mapping = aes(label = Label), data = subset(df, ! is.na(Label)), size = label_size)
  }

  if (return == "plot") return(p)
  if (return == "data") return(df)
  return(list(plot = p, data = df))

}
