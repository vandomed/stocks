#' Plot One Performance Metric over Time or One vs. Another over Time
#' 
#' Useful for assessing how one or two performance metrics vary over time, for 
#' one or several funds. Supports fixed-width rolling windows, fixed-width 
#' disjoint windows, and disjoint windows on per-month or per-year basis.
#' 
#' Much like \code{\link{plot_metrics}}, there are three ways to use the 
#' function:
#' 
#' \enumerate{
#'   \item Specify \code{tickers}. Function downloads data, calculates 
#'   performance metrics, and generates plot.
#'   \item Specify \code{gains} or \code{prices}. Function calculates 
#'   performance metrics and generates plot.
#'   \item Specify \code{metrics}. Function generates plot.
#' }
#' 
#' (1) is easiest, while (2) and (3) skip downloading data, which saves time 
#' when you're making multiple plots. Approaches (2) and (3) are good for 
#' piping.
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
#' @param tickers Character vector of ticker symbols that Yahoo! Finance 
#' recognizes, if you want to download data on the fly.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param gains Data frame with a date variable named Date and one column of 
#' gains for each investment.
#' @param prices Data frame with a date variable named Date and one column of 
#' prices for each investment.
#' @param benchmark,y.benchmark.x.benchmark Character string specifying which 
#' fund to use as benchmark for metrics (if you request \code{alpha}, 
#' \code{alpha.annualized}, \code{beta}, or \code{r.squared}).
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
                                  tickers = NULL, ..., 
                                  gains = NULL, 
                                  prices = NULL, 
                                  benchmark = "SPY", 
                                  y.benchmark = benchmark, 
                                  x.benchmark = benchmark, 
                                  return = "plot") {
  
  # Extract info from formula
  all.metrics <- all.vars(formula, functions = FALSE)
  y.metric <- x.metric <- NULL
  if (all.metrics[1] != ".") y.metric <- all.metrics[1]
  if (all.metrics[2] != ".") x.metric <- all.metrics[2]
  all.metrics <- c(x.metric, y.metric)
  
  xlabel <- metric.info$label[x.metric]
  ylabel <- metric.info$label[y.metric]
  labels <- c(xlabel, ylabel)
  
  # Align benchmarks with metrics
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% y.metric)) {
    y.benchmark <- NULL
  }
  if (! any(c("alpha", "alpha.annualized", "beta", "r.squared", "pearson", "spearman") %in% x.metric)) {
    x.benchmark <- NULL
  }
  
  # Check that requested metrics are valid
  invalid.requests <- setdiff(all.metrics, names(metric.info$label))
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
        
        gains <- load_gains(tickers = unique(c(y.benchmark, x.benchmark, tickers)), 
                            mutual.start = TRUE, mutual.end = TRUE, ...)
        
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
      
      if (type == "hop.year") {
        gains.long$Period <- year(gains.long$Date)
      } else if (type == "hop.month") {
        gains.long$Period <- paste(year(gains.long$Date), month(gains.long$Date, label = TRUE), sep = "-")
      } else {
        width <- as.numeric(substr(type, 5, 10))
        gains.long$Period <- rep(rep(1: ceiling(nrow(gains) / width), each = width)[1: nrow(gains)], length(tickers))
      }
      
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
          gain = Gain, metric = y.metric, width = width, units.year = units.year, benchmark.gains = get(y.benchmark)
        ), Fund][[2]]
      }
      
      if (! is.null(x.metric)) {
        df[[xlabel]] <- gains.long[, rolling_metric(
          gain = Gain, metric = x.metric, width = width, units.year = units.year, benchmark.gains = get(x.benchmark)
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
    
    p <- ggplot(df, aes(y = .data[[ylabel]], x = Date, group = Fund, color = Fund)) + 
      geom_point() + 
      geom_path() + 
      labs(title = paste(metric.info$title[y.metric], "over Time"),
           y = metric.info$label[y.metric], 
           x = "End date") + 
      ylim(range(c(0, df[[ylabel]]))) + 
      theme_bw()
    
  } else if (is.null(y.metric)) {
    
    p <- ggplot(df, aes(y = Date, x = .data[[xlabel]], group = Fund, color = Fund)) + 
      geom_point() + 
      geom_path() + 
      labs(title = paste(metric.info$title[y.metric], "over Time"),
           y = "End date", 
           x = xlabel) + 
      xlim(range(c(0, df[[xlabel]]))) +
      theme_bw()
    
  } else {
    
    p <- ggplot(df, aes(x = .data[[xlabel]], y = .data[[ylabel]], 
                        group = Fund, color = Fund)) + 
      geom_path() + 
      geom_point() + 
      geom_point(data = df[, .SD[1], Fund], show.legend = FALSE) + 
      geom_path(data = df[, .SD[c(.N-1, .N)], Fund], show.legend = FALSE, 
                arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
      labs(title = paste(metric.info$title[y.metric], "vs.", metric.info$title[x.metric]),
           y = ylabel, 
           x = xlabel) + 
      ylim(range(c(0, df[[ylabel]]))) + 
      xlim(range(c(0, df[[xlabel]]))) +  
      theme_bw()
    
  }
  
  if (return == "plot") return(p)
  if (return == "data") return(df)
  if (return == "both") return(list(plot = p, data = df))
  
}
