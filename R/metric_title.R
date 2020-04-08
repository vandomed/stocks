#' Get Title for Performance Metric
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
metric_title <- function(metric) {

  unlist(sapply(metric, function(x) {

    # Metrics
    if (x == "cagr") return("CAGR")
    if (x == "mdd") return("Max Drawdown")
    if (x == "mean") return("Mean")
    if (x == "sd") return ("SD")
    if (x == "sharpe") return ("Sharpe Ratio")
    if (x == "sortino") return("Sortino Ratio")
    if (grepl("growth.", x, fixed = TRUE)) {
      initial <- strsplit(x, "[.]")[[1]][2]
      if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
      return("Growth of $", comma(initial))
    }
    if (x == "growth") return("Growth")
    if (x == "alpha") return("Alpha")
    if (x == "alpha.annualized") return("Annualized Alpha")
    if (x == "beta") return("Beta")
    if (x == "r.squared") return("R-squared")
    if (x == "r") return("Correlation")
    if (x == "rho") return("Spearman Correlation")
    if (x == "r.auto") return("Autocorrelation")
    if (x == "rho.auto") return("Spearman Autocorrelation")

    # Other types of variables
    if (x == "allocation") return("Allocation")
    if (x == "time") return("Time Period")

    return(x)

  }))

}
