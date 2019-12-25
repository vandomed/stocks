#' Calculate Performance Metric
#'
#' Mainly a helper function for \code{\link{plot_metrics_overtime}}.
#'
#'
#' @param gains Numeric vector.
#' @param metric Character string specifying metric to calculate. Choices are
#' \code{"mean"}, \code{"sd"}, \code{"growth.x"} for growth of $x where x is the
#' initial value, \code{"growth"} for percent growth, \code{"cagr"} for compound
#' annualized growth rate, \code{"mdd"} for max drawdown, \code{"sharpe"},
#' \code{"sortino"}, \code{"alpha"}, \code{"alpha.annualized"}, \code{"beta"},
#' \code{"r.squared"}, \code{"pearson"} or \code{"spearman"} for
#' Pearson/Spearman correlation with benchmark, and \code{"auto.pearson"} or
#' \code{"auto.spearman"} for Pearson/Spearman autocorrelation.
#' @param units.year Integer value.
#' @param benchmark.gains Numeric vector.
#'
#'
#' @return
#' Numeric value.
#'
#'
#' @export
calc_metric <- function(gains,
                        metric = "mean",
                        units.year = 252,
                        benchmark.gains = NULL) {

  if (metric == "mean") {
    return(mean(gains) * 100)
  }
  if (metric == "sd") {
    return(sd(gains) * 100)
  }
  if (grepl("[.]", metric)) {
    initial <- strsplit(metric, "[.]")[[1]][2]
    if (grepl("k", initial)) {
      initial <- as.numeric(strsplit(initial, "k")[[1]][1]) * 1000
    } else {
      initial <- as.numeric(initial)
    }
    return(prod(gains + 1) * initial)
  }
  if (metric == "growth") {
    return(gains_rate(gains) * 100)
  }
  if (metric == "cagr") {
    return(gains_rate(gains, units.year) * 100)
  }
  if (metric == "mdd") {
    return(mdd(gains = as.numeric(gains)) * 100)
  }
  if (metric == "sharpe") {
    return(sharpe(gains))
  }
  if (metric == "sortino") {
    return(sortino(gains))
  }
  if (metric == "alpha") {
    return(lm(gains ~ benchmark.gains)$coef[1] * 100)
  }
  if (metric == "alpha.annualized") {
    return(convert_gain(lm(gains ~ benchmark.gains)$coef[1], 1, units.year) * 100)
  }
  if (metric == "beta") {
    return(lm(gains ~ benchmark.gains)$coef[2])
  }
  if (metric == "r.squared") {
    return(summary(lm(gains ~ benchmark.gains))$r.squared)
  }
  if (metric == "pearson") {
    return(cor(gains, benchmark.gains))
  }
  if (metric == "spearman") {
    return(cor(gains, benchmark.gains, method = "spearman"))
  }
  if (metric == "auto.pearson") {
    return(cor(gains[-length(gains)], gains[-1]))
  }
  if (metric == "auto.spearman") {
    return(cor(gains[-length(gains)], gains[-1], method = "spearman"))
  }
}
