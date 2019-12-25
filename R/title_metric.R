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

  if (title == "Mean") return("mean")
  if (title == "SD") return ("sd")
  if (grepl("Growth of", title)) return(paste("growth", strsplit(title, "[$]")[[1]][2], sep = "."))
  if (title == "Growth") return("growth")
  if (title == "CAGR") return("cagr")
  if (title == "Max Drawdown") return("mdd")
  if (title == "Sharpe Ratio") return ("sharpe")
  if (title == "Sortino Ratio") return("sortino")
  if (title == "Alpha") return("alpha")
  if (title == "Annualized Alpha") return("alpha.annualized")
  if (title == "Beta") return("beta")
  if (title == "R-squared") return("r.squared")
  if (title == "Pearson Corr.") return("pearson")
  if (title == "Spearman Corr.") return("spearman")
  if (title == "Pearson Autocorr.") return("auto.pearson")
  if (title == "Spearman Autocorr.") return("auto.spearman")
  if (title == "Allocation") return("allocation")

}
