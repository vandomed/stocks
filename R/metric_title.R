#' Get Title for Performance Metric
#'
#' For internal use only.
#'
#'
#' @param metric Character string.
#'
#'
#' @return
#' Character string.
#'
#'
metric_title <- function(metric) {

  if (metric == "mean") return("Mean")
  if (metric == "sd") return ("SD")
  if (grepl("[.]", metric)) {
    initial <- strsplit(metric, "[.]")[[1]][2]
    if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
    return("Growth of $", comma(initial))
  }
  if (metric == "growth") return("Growth")
  if (metric == "cagr") return("CAGR")
  if (metric == "mdd") return("Max Drawdown")
  if (metric == "sharpe") return ("Sharpe Ratio")
  if (metric == "sortino") return("Sortino Ratio")
  if (metric == "alpha") return("Alpha")
  if (metric == "alpha.annualized") return("Annualized Alpha")
  if (metric == "beta") return("Beta")
  if (metric == "r.squared") return("R-squared")
  if (metric == "pearson") return("Pearson Corr.")
  if (metric == "spearman") return("Spearman Corr.")
  if (metric == "auto.pearson") return("Pearson Autocorr.")
  if (metric == "auto.spearman") return("Spearman Autocorr.")
  if (metric == "allocation") return("Allocation")

}
