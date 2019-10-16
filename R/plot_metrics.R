#' Plot One Performance Metric (Sorted Bar Plot) or One vs. Another 
#' (Scatterplot)
#' 
#' Useful for comparing funds on one or more metrics.
#' 
#' 
#' @param metrics "Long" data frame with Fund column and column for each metric 
#' you want to plot. Typically the result of a prior call to 
#' \code{\link{calc_metrics}}. 
#' @param formula Formula specifying what to plot, e.g. \code{cagr ~ mdd} for 
#' CAGR vs. MDD, \code{cagr ~ .} for just CAGR, or \code{. ~ mdd} for just MDD.
#' See \code{?calc_metrics} for list of metrics to choose from.
#' @param tickers Character vector of ticker symbols that Yahoo! Finance 
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param gains Data frame with one column of gains for each investment and a 
#' date variable named Date.
#' @param prices Data frame with one column of prices for each investment and a 
#' date variable named Date.
#' @param benchmark Character string specifying which fund to use as a 
#' benchmark for metrics that require one.
#' @param y.benchmark Character string specifying which fund to use as benchmark 
#' for y-axis metric.
#' @param x.benchmark Character string specifying which fund to use as benchmark 
#' for x-axis metric.
#' @param ggplotly Logical value for whether to convert the 
#' \code{\link[ggplot2]{ggplot}} to a \code{\link[plotly]{ggplotly}} object 
#' internally.
#' @param return Character string specifying what to return. Choices are 
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#' 
#' 
#' @return
#' Depending on \code{return}, a \code{\link[ggplot2]{ggplot}}, a data frame 
#' with the source data, or a list containing both.
#' 
#' 
#' @references 
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial 
#' Modelling Framework. R package version 0.4-15. 
#' \url{https://CRAN.R-project.org/package=quantmod}
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot CAGR vs. MDD for SPY and BRK-B, downloading data on the fly
#' plot_metrics(formula = cagr ~ mdd, tickers = c("SPY", "BRK-B"))
#' 
#' # Plot Sharpe ratio for FANG stocks, in more explicit steps with piping
#' plot_metrics(formula = . ~ sharpe, tickers = c("FB", "AAPL", "NFLX", "GOOG"))
#' }
#'   
#'
#' @export
plot_metrics <- function(metrics = NULL, 
                         formula = cagr ~ mdd, 
                         tickers = NULL, 
                         ..., 
                         gains = NULL,
                         prices = NULL, 
                         benchmark = "SPY", 
                         y.benchmark = benchmark, 
                         x.benchmark = benchmark, 
                         ggplotly = FALSE, 
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
  all.metrics <- c(y.metric, x.metric)
  
  xlabel <- metric.info$label[x.metric]
  ylabel <- metric.info$label[y.metric]
  
  # Set benchmarks to NULL if not needed
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% all.metrics)) {
    benchmark <- y.benchmark <- x.benchmark <- NULL
  }
  
  # Check that requested metrics are valid
  invalid.requests <- setdiff(all.metrics, names(metric.info$label))
  if (length(invalid.requests) > 0) {
    stop(paste("The following metrics are not allowed (see ?metrics for choices):", 
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
        
        gains <- load_gains(tickers = unique(c(y.benchmark, x.benchmark, tickers)), 
                            mutual.start = TRUE, mutual.end = TRUE, ...)
        
      } else {
        
        stop("You must specify 'metrics', 'gains', 'prices', or 'tickers'")
        
      }
      
    }
    
    # If tickers is NULL, set to all funds in gains, excluding benchmarks
    if (is.null(tickers)) {
      tickers <- setdiff(names(gains), c("Date", y.benchmark, x.benchmark))
    }
    
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
    
    df <- data.frame(Fund = tickers)
    
    if (! is.null(y.metric)) {
      df[[ylabel]] <- sapply(gains[tickers], function(x) {
        calc_metric(gains = x, metric = y.metric, units.year = units.year, benchmark.gains = y.benchmark.gains)
      })
    }
    
    if (! is.null(x.metric)) {
      df[[xlabel]] <- sapply(gains[tickers], function(x) {
        calc_metric(gains = x, metric = x.metric, units.year = units.year, benchmark.gains = x.benchmark.gains)
      })
    }
    
  } else {
    df <- as.data.frame(metrics)
  }
  
  # Drop benchmarks
  df <- subset(df, ! Fund %in% c(y.benchmark, x.benchmark))

  # Create plot
  
  if (is.null(x.metric)) {
    
    # For y.metric only
    df$text <- paste("Fund: ", df$Fund,
                     "<br>", ylabel, ": ", round(df[[ylabel]], metric.info$decimals[y.metric]), sep = "") 
    p <- ggplot(df, aes(y = .data[[ylabel]], 
                        x = reorder(Fund, .data[[ylabel]]), 
                        text = text)) + 
      geom_col() + 
      labs(title = paste(metric.info$title[y.metric], "for Various Funds"),
           y = ylabel, x = NULL)
    
  } else if (is.null(y.metric)) {
    
    # For x.metric only
    df$text <- paste("Fund: ", df$Fund,
                     "<br>", xlabel, ": ", round(df[[xlabel]], metric.info$decimals[x.metric]), sep = "") 
    p <- ggplot(df, aes(y = .data[[xlabel]], x = reorder(Fund, .data[[xlabel]]), 
                        text = text)) + 
      geom_col() + 
      coord_flip() + 
      labs(title = paste(metric.info$title[x.metric], "for Various Funds"),
           y = ylabel, x = NULL)
    
  } else {
    
    df$text <- paste("Fund: ", df$Fund,
                     "<br>", xlabel, ": ", round(df[[xlabel]], metric.info$decimals[x.metric]), 
                     "<br>", ylabel, ": ", round(df[[ylabel]], metric.info$decimals[y.metric]), sep = "") 
    p <- ggplot(df, aes(y = .data[[ylabel]], 
                        x = .data[[xlabel]], 
                        group = Fund, label = Fund, text = text)) +
      geom_point() +
      geom_label_repel() +
      ylim(range(c(0, df[[ylabel]])) * 1.02) +
      xlim(range(c(0, df[[xlabel]])) * 1.02) +
      labs(title = paste(metric.info$title[y.metric], "vs.", metric.info$title[x.metric]),
           y = ylabel, x = xlabel)
    
  }
  
  if (ggplotly) p <- ggplotly(p, tooltip = "text")
  df <- df[names(df) != "text"]
  
  if (return == "plot") return(p)
  if (return == "data") return(df)
  if (return == "both") return(list(plot = p, data = df))
  
}
