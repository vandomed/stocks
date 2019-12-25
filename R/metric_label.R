#' Get Label for Performance Metric
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
metric_label <- function(metric) {
  sapply(metric, function(x) {
    if (x == "mean") return("Mean (%)")
    if (x == "sd") return ("SD (%)")
    if (grepl("[.]", x)) {
      initial <- strsplit(x, "[.]")[[1]][2]
      if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
      return(paste("Growth of $", comma(as.numeric(initial)), sep = ""))
    }
    if (x == "growth") return("Growth (%)")
    if (x == "cagr") return("CAGR (%)")
    if (x == "mdd") return("Max drawdown (%)")
    if (x == "sharpe") return ("Sharpe ratio")
    if (x == "sortino") return("Sortino ratio")
    if (x == "alpha") return("Alpha")
    if (x == "alpha.annualized") return("Annualized alpha")
    if (x == "beta") return("Beta")
    if (x == "r.squared") return("R-squared")
    if (x == "pearson") return("Pearson corr.")
    if (x == "spearman") return("Spearman corr.")
    if (x == "auto.pearson") return("Pearson autocorr.")
    if (x == "auto.spearman") return("Spearman autocorr.")
    if (x == "allocation") return("Allocation (%)")
  })
}
# metric_label <- function(metric) {
#
#   if (metric == "mean") return("Mean (%)")
#   if (metric == "sd") return ("SD (%)")
#   if (grepl("[.]", metric)) {
#     initial <- strsplit(metric, "[.]")[[1]][2]
#     if (grepl("k", initial)) return(paste("Growth of $", initial, sep = ""))
#     return("Growth of $", comma(initial))
#   }
#   if (metric == "growth") return("Growth (%)")
#   if (metric == "cagr") return("CAGR (%)")
#   if (metric == "mdd") return("Max drawdown (%)")
#   if (metric == "sharpe") return ("Sharpe ratio")
#   if (metric == "sortino") return("Sortino ratio")
#   if (metric == "alpha") return("Alpha")
#   if (metric == "alpha.annualized") return("Annualized alpha")
#   if (metric == "beta") return("Beta")
#   if (metric == "r.squared") return("R-squared")
#   if (metric == "pearson") return("Pearson corr.")
#   if (metric == "spearman") return("Spearman corr.")
#   if (metric == "auto.pearson") return("Pearson autocorr.")
#   if (metric == "auto.spearman") return("Spearman autocorr.")
#   if (metric == "allocation") return("Allocation (%)")
#
# }
