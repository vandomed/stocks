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
    if (x == "mean") return("Mean")
    if (x == "sd") return ("SD")
    if (grepl("growth.", x, fixed = TRUE)) {
      initial <- strsplit(x, "[.]")[[1]][2]
      if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
      return("Growth of $", comma(initial))
    }
    if (x == "growth") return("Growth")
    if (x == "cagr") return("CAGR")
    if (x == "mdd") return("Max Drawdown")
    if (x == "sharpe") return ("Sharpe Ratio")
    if (x == "sortino") return("Sortino Ratio")
    if (x == "alpha") return("Alpha")
    if (x == "alpha.annualized") return("Annualized Alpha")
    if (x == "beta") return("Beta")
    if (x == "r.squared") return("R-squared")
    if (x == "pearson") return("Pearson Corr.")
    if (x == "spearman") return("Spearman Corr.")
    if (x == "auto.pearson") return("Pearson Autocorr.")
    if (x == "auto.spearman") return("Spearman Autocorr.")
    if (x == "allocation") return("Allocation")
  }))

}
