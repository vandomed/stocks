#' Plot One Performance Metric vs. Another for Two-Fund Portfolios
#' 
#' Useful for visualizing the behavior of two-fund portfolios, often by plotting 
#' a measure of growth vs. a measure of volatility.
#' 
#' 
#' @param metrics Data frame with Fund column and column for each metric you 
#' want to plot. Typically the result of a prior call to 
#' \code{\link{calc_metrics_2funds}}. 
#' @param formula Formula specifying what to plot, e.g. \code{mean ~ sd}, 
#' \code{cagr ~ mdd}, or \code{sharpe ~ allocation}. See \code{?calc_metrics} 
#' for list of metrics to choose from (\code{"allocation"} is an extra option 
#' here). If you specify \code{metrics}, default behavior is to use 
#' \code{mean ~ sd} unless either is not available, in which case the first two 
#' performance metrics that appear as columns in \code{metrics} are used.
#' @param tickers Character vector of ticker symbols, where the first two are 
#' are a two-fund pair, the next two are another, and so on.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param gains Data frame with a date variable named Date and one column of 
#' gains for each investment.
#' @param prices Data frame with a date variable named Date and one column of 
#' prices for each investment.
#' @param benchmark,y.benchmark,x.benchmark Character string specifying which 
#' fund to use as benchmark for metrics (if you request \code{alpha}, 
#' \code{alpha.annualized}, \code{beta}, or \code{r.squared}).
#' @param ref.tickers Character vector of ticker symbols to include on the 
#' plot.
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
#' # Plot mean vs. SD for UPRO/VBLTX, and compare to SPY
#' plot_metrics_2funds(mean ~ sd, c("UPRO", "VBLTX"))
#' 
#' # Plot CAGR vs. MDD for AAPL/GOOG and FB/TWTR
#' plot_metrics_2funds(cagr ~ mdd, tickers = c("AAPL", "GOOG", "FB", "TWTR"))
#' 
#' # Plot Sharpe ratio vs. allocation for SPY/TLT
#' plot_metrics_2funds(sharpe ~ allocation, tickers = c("SPY", "TLT"))
#' }
#'
#' @export
plot_metrics_2funds <- function(metrics = NULL, 
                                formula = mean ~ sd, 
                                tickers = NULL, ..., 
                                gains = NULL, 
                                prices = NULL, 
                                benchmark = "SPY", 
                                y.benchmark = benchmark, 
                                x.benchmark = benchmark, 
                                ref.tickers = "SPY", 
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
  
  # Prep for calculating metrics and plotting
  ylabel <- metric.info$label[y.metric]
  xlabel <- metric.info$label[x.metric]
  
  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% all.metrics)) {
    benchmark <- y.benchmark <- x.benchmark <- NULL
  }
  
  # Check that requested metrics are valid
  invalid.requests <- setdiff(all.metrics, names(metric.info$label))
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
    
    # Create/update tickers (should not include Date or benchmark/reference tickers)
    if (is.null(tickers)) tickers <- names(gains)
    tickers <- setdiff(tickers, c("Date", y.benchmark, x.benchmark, ref.tickers))
    
    # Drop NA's
    gains <- gains[complete.cases(gains), , drop = FALSE]
    
    # Figure out conversion factor in case CAGR or annualized alpha is requested
    min.diffdates <- min(diff(unlist(head(gains$Date, 10))))
    units.year <- ifelse(min.diffdates == 1, 252, ifelse(min.diffdates <= 30, 12, 1))
    
    # Extract gains for benchmark index
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
      if (y.metric != "allocation") {
        df.pair[[ylabel]] <- apply(wgains.pair, 2, function(x) {
          calc_metric(gains = x, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
        })
      }
      if (x.metric != "allocation") {
        df.pair[[xlabel]] <- apply(wgains.pair, 2, function(x) {
          calc_metric(gains = x, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
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
    
  } else {
    df <- metrics
  }
  
  # Prep for ggplot
  #df <- df[c("Pair", "Label", unique(c("Allocation (%)", xlabel, ylabel)))]
  df <- as.data.frame(df)
  df.points <- subset(df, Pair %in% ref.tickers | `Allocation (%)` %in% seq(0, 100, 10))
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1: n]
  }
  cols <- c()
  pairs <- setdiff(unique(df$Pair), ref.tickers)
  cols[pairs] <- gg_color_hue(length(pairs))
  cols[ref.tickers] <- "black"
  
  p <- ggplot(df, mapping = aes(x = .data[[xlabel]], y = .data[[ylabel]], group = Pair, color = Pair)) + 
    labs(title = paste(metric.info$title[y.metric], "vs.", metric.info$title[x.metric]),
         y = ylabel, 
         x = xlabel) + 
    ylim(range(c(0, df[[ylabel]])) * 1.01) + 
    xlim(range(c(0, df[[xlabel]])) * 1.01) + 
    scale_colour_manual(values = cols) +
    theme_bw()
  
  if (x.metric == "allocation" & ! is.null(ref.tickers)) {
    p <- p + geom_hline(data = df.ref, yintercept = df.ref[[ylabel]], lty = 2)
  } else if (y.metric == "allocation" & ! is.null(ref.tickers)) {
    p <- p + geom_vline(data = df.ref, yintercept = df.ref[[xlabel]], lty = 2)
  }
  
  p <- p + 
    geom_point(data = df.points, show.legend = FALSE) +
    geom_path(show.legend = FALSE) + 
    geom_label_repel(aes(label = Label), data = subset(df, ! is.na(Label)), show.legend = FALSE) 
  
  if (return == "plot") return(p)
  if (return == "data") return(df[, ! names(df) %in% c("Fund 1", "Fund 2", "Allocation (%)")])
  if (return == "both") return(list(plot = p, data = df[, ! names(df) %in% c("Fund 1", "Fund 2", "Allocation (%)")]))
  
}
