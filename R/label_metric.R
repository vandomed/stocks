#' Convert Label back to Performance Metric
#'
#' Mainly a helper function.
#'
#'
#' @param label Character string.
#'
#'
#' @return
#' Character string.
#'
#'
#' @export
label_metric <- function(label) {

  unlist(sapply(label, function(x) {

    # Metrics
    if (x == "Mean (%)") return("mean")
    if (x == "SD (%)") return ("sd")
    if (grepl("Growth of", x)) return(paste("growth", strsplit(x, "[$]")[[1]][2], sep = "."))
    if (x == "Growth (%)") return("growth")
    if (x == "CAGR (%)") return("cagr")
    if (x == "Max drawdown (%)") return("mdd")
    if (x == "Sharpe ratio") return ("sharpe")
    if (x == "Sortino ratio") return("sortino")
    if (x == "Alpha (%)") return("alpha")
    if (x == "Annualized alpha (%)") return("alpha.annualized")
    if (x == "Beta") return("beta")
    if (x == "R-squared") return("r.squared")
    if (x == "Pearson corr.") return("pearson")
    if (x == "Spearman corr.") return("spearman")
    if (x == "Pearson autocorr.") return("auto.pearson")
    if (x == "Spearman autocorr.") return("auto.spearman")

    # Other types of variables
    if (x == "Allocation (%)") return("allocation")
    if (x == "Time period") return("time")
    if (x == "Metric") return("metric")
    if (x == "Set") return("set")

  }))

}
