#' Convert Title back to Performance Metric
#'
#' For internal use only.
#'
#'
#' @param title Character string.
#'
#'
#' @return
#' Character string.
#'
#'
title_metric <- function(title) {

  sapply(title, function(x) {
    if (x == "Mean") return("mean")
    if (x == "SD") return ("sd")
    if (grepl("Growth of", x)) return(paste("growth", strsplit(x, "[$]")[[1]][2], sep = "."))
    if (x == "Growth") return("growth")
    if (x == "CAGR") return("cagr")
    if (x == "Max Drawdown") return("mdd")
    if (x == "Sharpe Ratio") return ("sharpe")
    if (x == "Sortino Ratio") return("sortino")
    if (x == "Alpha") return("alpha")
    if (x == "Annualized Alpha") return("alpha.annualized")
    if (x == "Beta") return("beta")
    if (x == "R-squared") return("r.squared")
    if (x == "Pearson Corr.") return("pearson")
    if (x == "Spearman Corr.") return("spearman")
    if (x == "Pearson Autocorr.") return("auto.pearson")
    if (x == "Spearman Autocorr.") return("auto.spearman")
    if (x == "Allocation") return("allocation")
    if (x == "Time Period") return("time")
  })

}
