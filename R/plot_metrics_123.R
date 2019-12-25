#' Plot One Performance Metric vs. Another for Any Combination of Individual
#' Funds, 2-Fund Portfolios, and 3-Fund Portfolios
#'
#'
#' Integrates \code{plot_metrics}, \code{plot_metrics_2funds}, and
#' \code{plot_metrics_3funds} into a single function, so you can visualize
#' strategies of varying complexities on one figure. Supports a wide range of
#' portfolio visualizations.
#'
#' If you prefer to have complete control over the plotting, you can set
#' \code{return = "data"} to just get the source data.
#'
#'
#' @param metrics Data frame with Fund column and column for each metric you
#' want to plot. Typically the result of a prior call to
#' \code{\link{calc_metrics_123}}.
#' @param formula Formula specifying what to plot, e.g. \code{mean ~ sd},
#' \code{cagr ~ mdd}, or \code{sharpe ~ allocation}. See \code{?calc_metrics}
#' for list of metrics to choose from (\code{"allocation"} is an extra option
#' here). If you specify \code{metrics}, default behavior is to use
#' \code{mean ~ sd} unless either is not available, in which case the first two
#' performance metrics that appear as columns in \code{metrics} are used.
#' @param tickers Character vector of ticker symbols, where the first three are
#' are a three-fund set, the next three are another, and so on.
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
#' # Plot CAGR vs. max drawdown for BRK-B, SPY/TLT, and VWEHX/VBLTX/VFINX
#' plot_metrics_123(
#'   formula = cagr ~ mdd,
#'   tickers = list("BRK-B", c("SPY", "TLT"), c("VWEHX", "VBLTX", "VFINX"))
#' )
#' }
#'
#'
#'
#' @export
# metrics = NULL
# formula = sharpe ~ cagr
# tickers = list(c("SPY", "VBLTX", "VWEHX"), c("UPRO", "VBLTX", "VWEHX"))
# from = "2010-01-01"
# base_size = 15
# label_size = 5
# step.between = 2
# step.along = 1
# plotly = TRUE
#
# gains = NULL
# prices = NULL
# benchmark = "SPY"
# y.benchmark = benchmark
# x.benchmark = benchmark
# title = NULL
# return = "plot"

plot_metrics_123 <- function(metrics = NULL,
                             formula = mean ~ sd,
                             tickers = NULL, ...,
                             step = 1,
                             gains = NULL,
                             prices = NULL,
                             benchmark = "SPY",
                             y.benchmark = benchmark,
                             x.benchmark = benchmark,
                             plotly = FALSE,
                             title = NULL,
                             base_size = 16,
                             label_size = 6,
                             return = "plot") {

  # Extract info from formula
  all.metrics <- all.vars(formula, functions = FALSE)
  if (! is.null(metrics) & ! all(metric.info$label[all.metrics] %in% names(metrics))) {
    all.metrics <- names(metric.info$label[metric.info$label %in% intersect(names(metrics), metric.info$label)])
    if (length(all.metrics) >= 2) {
      all.metrics <- all.metrics[1: 2]
    } else {
      stop("The input 'metrics' must have at least two columns with performance metrics")
    }
  }
  y.metric <- x.metric <- NULL
  if (all.metrics[1] != ".") y.metric <- all.metrics[1]
  if (all.metrics[2] != ".") x.metric <- all.metrics[2]
  all.metrics <- c(y.metric, x.metric)

  ylabel <- metric.info$label[y.metric]
  xlabel <- metric.info$label[x.metric]

  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% all.metrics)) {
    benchmark <- y.benchmark <- x.benchmark <- NULL
  }

  # Check that requested metrics are valid
  invalid.requests <- setdiff(all.metrics, names(metric.info[[1]]))
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?calc_metrics for choices):",
               paste(invalid.requests, collapse = ", ")))
  }

  # Calculate performance metrics if not pre-specified
  if (is.null(metrics)) {

    # Determine gains if not pre-specified
    if (is.null(gains)) {

      if (! is.null(prices)) {

        date.var <- names(prices) == "Date"
        gains <- cbind(prices[-1, date.var, drop = FALSE],
                       sapply(prices[! date.var], pchanges))

      } else if (! is.null(tickers)) {

        gains <- load_gains(tickers = unique(c(y.benchmark, x.benchmark, unlist(tickers))),
                            mutual.start = TRUE, mutual.end = TRUE, ...)

      } else {

        stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")

      }

    }

    # If tickers is NULL, set to all single funds other than benchmarks
    if (is.null(tickers)) tickers <- as.list(setdiff(names(gains), c("Date", y.benchmark, x.benchmark)))

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
        df.x[[ylabel]] <- calc_metric(gains = gains.x, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
        df.x[[xlabel]] <- calc_metric(gains = gains.x, metric = x.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
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
        df.x[[ylabel]] <- apply(wgains, 2, function(y) {
          calc_metric(gains = y, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
        })
        df.x[[xlabel]] <- apply(wgains, 2, function(y) {
          calc_metric(gains = y, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
        })
        return(df.x)

      }

      gains.x <- as.matrix(gains[x])
      weights <- do.call(cbind, sapply(seq(0, 100, step), function(c1) {
        c2 <- unique(c(seq(0, 100 - c1, step), 100 - c1))
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
      df.x[[ylabel]] <- apply(wgains, 2, function(y) {
        calc_metric(gains = y, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
      })
      df.x[[xlabel]] <- apply(wgains, 2, function(y) {
        calc_metric(gains = y, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
      })
      return(df.x)

    }))

  } else {
    df <- metrics
  }

  # Prep for ggplot
  df$tooltip <- paste(
    ifelse(df$Funds == 1, df$Set,
    ifelse(df$Funds == 2, paste(df$`Allocation 1 (%)`, "% ", df$`Fund 1`, ", ", df$`Allocation 2 (%)`, "% ", df$`Fund 2`, sep = ""),
    paste(df$`Allocation 1 (%)`, "% ", df$`Fund 1`, ", ", df$`Allocation 2 (%)`, "% ", df$`Fund 2`, ", ", df$`Allocation 3 (%)`, "% ", df$`Fund 3`, sep = ""))),
    "<br>", metric.info$title[x.metric], ": ", formatC(df[[xlabel]], metric.info$decimals[x.metric], format = "f"), metric.info$units[x.metric],
    "<br>", metric.info$title[y.metric], ": ", formatC(df[[ylabel]], metric.info$decimals[y.metric], format = "f"), metric.info$units[y.metric], sep = ""
  )

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1: n]
  }
  sets <- unique(df$Set)
  ns <- sapply(sets, function(x) df$Funds[which.max(df$Set == x)])
  cols <- ifelse(ns == 1, "black", gg_color_hue(sum(ns != 1)))

  # Create plot
  p <- ggplot(df, aes(y = .data[[ylabel]], x = .data[[xlabel]],
                      group = Set, colour = Set, label = Label, text = tooltip))
  for (ii in 1: length(ns)) {

    n <- ns[ii]

    if (n == 1) {
      next
    }

    if (n == 2) {
      p <- p + geom_path(data = subset(df, Set == sets[ii]))
      next
    }

    if (n == 3) {
      df.subset <- subset(df, Set == sets[ii])
      p <- p +
        geom_path(data = subset(df.subset, `Allocation 2 (%)` == 0), col = "black") +
        geom_path(data = subset(df.subset, `Allocation 3 (%)` == 0), col = "black") +
        geom_path(data = subset(df, Set == sets[ii] & `Allocation 1 (%)` != 0),
                  mapping = aes(group = interaction(Set, `Allocation 1 (%)`)), alpha = 0.5) +
        geom_path(data = subset(df.subset, `Allocation 1 (%)` == 0), col = "black")
    }

  }

  p <- p +
    geom_point(data = subset(df, `Allocation 1 (%)` == 100 | `Allocation 2 (%)` == 100 | `Allocation 3 (%)` == 100), col = "black") +
    ylim(range(c(0, df[[ylabel]])) * 1.01) +
    xlim(range(c(0, df[[xlabel]])) * 1.01) +
    scale_colour_manual(values = cols) +
    theme_gray(base_size = base_size) +
    theme(legend.position = "none") +
    labs(title = ifelse(! is.null(title), title, paste(metric.info$title[y.metric], "vs.", metric.info$title[x.metric])),
         y = ylabel, x = xlabel)

  if (plotly) {
    p <- ggplotly(p, tooltip = "tooltip") %>%
      style(hoverlabel = list(font = list(size = 15)))
  } else {
    p <- p + geom_label_repel(data = subset(df, ! is.na(Label)), size = label_size)
  }

  if (return == "plot") return(p)
  if (return == "data") return(df)
  return(list(plot = p, data = df))

}
