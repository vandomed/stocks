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
    if (x == "CAGR (%)") return("cagr")
    if (x == "Max drawdown (%)") return("mdd")
    if (x == "Mean (%)") return("mean")
    if (x == "SD (%)") return ("sd")
    if (x == "Sharpe ratio") return ("sharpe")
    if (x == "Sortino ratio") return("sortino")
    if (grepl("Growth of", x)) return(paste("growth", strsplit(x, "[$]")[[1]][2], sep = "."))
    if (x == "Growth (%)") return("growth")
    if (x == "Alpha (%)") return("alpha")
    if (x == "Annualized alpha (%)") return("alpha.annualized")
    if (x == "Beta") return("beta")
    if (x == "R-squared") return("r.squared")
    if (x == "Correlation") return("r")
    if (x == "Spearman correlation") return("rho")
    if (x == "Autocorrelation") return("r.auto")
    if (x == "Spearman autocorrelation") return("rho.auto")

    # Other types of variables
    if (x == "Allocation (%)") return("allocation")
    if (x %in% c("End date", "Time period")) return("time")
    if (x == "Metric") return("metric")
    if (x == "Set") return("set")

  }))

}
