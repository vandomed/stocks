#' Get Label for Performance Metric
#'
#' Mainly a helper function.
#'
#'
#' @param metric Character string.
#'
#'
#' @return
#' Character string.
#'
#'
#' @export
metric_label <- function(metric) {

  unlist(sapply(metric, function(x) {

    # Metrics
    if (x == "cagr") return("CAGR (%)")
    if (x == "mdd") return("Max drawdown (%)")
    if (x == "mean") return("Mean (%)")
    if (x == "sd") return ("SD (%)")
    if (x == "sharpe") return ("Sharpe ratio")
    if (x == "sortino") return("Sortino ratio")
    if (grepl("growth.", x, fixed = TRUE)) {
      initial <- strsplit(x, "[.]")[[1]][2]
      if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
      return(paste("Growth of $", comma(as.numeric(initial)), sep = ""))
    }
    if (x == "growth") return("Growth (%)")
    if (x == "alpha") return("Alpha (%)")
    if (x == "alpha.annualized") return("Annualized alpha (%)")
    if (x == "beta") return("Beta")
    if (x == "r.squared") return("R-squared")
    if (x == "r") return("Correlation")
    if (x == "rho") return("Spearman correlation")
    if (x == "r.auto") return("Autocorrelation")
    if (x == "rho.auto") return("Spearman autocorrelation")

    # Other types of variables
    if (x == "allocation") return("Allocation (%)")
    if (x == "time") return("Time period")
    if (x == "metric") return("Metric")
    if (x == "set") return("Set")

    return(x)

  }))
}
