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
    if (x == "Growth") return("growth")
    if (x == "CAGR") return("cagr")
    if (x == "Mean") return("mean")
    if (x == "SD") return ("sd")
    if (x == "Sharpe Ratio") return ("sharpe")
    if (x == "Sortino Ratio") return("sortino")
    if (grepl("Growth of", x)) return(paste("growth", strsplit(x, "[$]")[[1]][2], sep = "."))
    if (x == "Max Drawdown") return("mdd")
    if (x == "Alpha") return("alpha")
    if (x == "Annualized Alpha") return("alpha.annualized")
    if (x == "Beta") return("beta")
    if (x == "R-squared") return("r.squared")
    if (x == "Correlation") return("r")
    if (x == "Spearman Correlation") return("rho")
    if (x == "Autocorrelation") return("r.auto")
    if (x == "Spearman Autocorrelation") return("rho.auto")
    if (x == "Allocation") return("allocation")
    if (x == "Time Period") return("time")
  })

}
