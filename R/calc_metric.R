#' Calculate Performance Metric
#'
#' Mainly a helper function for \code{\link{calc_metrics}} and
#' \code{\link{calc_metrics_overtime}}, but could also be used independently.
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
#' @examples
#' \dontrun{
#' # Load daily gains for SPY in 2019 and calculate various metrics
#' gains <- load_gains(tickers = "SPY", from = "2019-01-01", to = "2019-12-31")
#' calc_metric(gains$SPY, "growth")
#' calc_metric(gains$SPY, "cagr")
#' calc_metric(gains$SPY, "mdd")
#' calc_metric(gains$SPY, "sharpe")
#' calc_metric(gains$SPY, "growth.10k")
#'
#' # Calculate alpha and beta for TLT in 2019, using SPY as a benchmark
#' gains <- load_gains(tickers = c("SPY", "TLT"), from = "2019-01-01", to = "2019-12-31")
#' calc_metric(gains = gains$TLT, metric = "alpha", benchmark.gains = gains$SPY)
#' calc_metric(gains = gains$TLT, metric = "beta", benchmark.gains = gains$SPY)
#' }
#'
#'
#' @export
calc_metric <- function(gains,
                        metric = "mean",
                        units.year = 252,
                        benchmark.gains = NULL) {

  if (metric == "cagr") {
    return(gains_rate(gains, units.year) * 100)
  }
  if (metric == "mdd") {
    return(mdd(gains = as.numeric(gains)) * 100)
  }
  if (metric == "mean") {
    return(mean(gains) * 100)
  }
  if (metric == "sd") {
    return(sd(gains) * 100)
  }
  if (metric == "sharpe") {
    return(sharpe(gains))
  }
  if (metric == "sortino") {
    return(sortino(gains))
  }
  if (grepl("growth.", metric)) {
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
  if (metric == "alpha") {
    return(fastLmPure(X = cbind(1, benchmark.gains), y = gains)$coef[1] * 100)
  }
  if (metric == "alpha.annualized") {
    return(convert_gain(fastLmPure(X = cbind(1, benchmark.gains), y = gains)$coef[1], 1, units.year) * 100)
  }
  if (metric == "beta") {
    return(fastLmPure(X = cbind(1, benchmark.gains), y = gains)$coef[2])
  }
  if (metric == "r.squared") {
    return(summary(lm(gains ~ benchmark.gains))$r.squared)
  }
  if (metric == "r") {
    return(cor(gains, benchmark.gains))
  }
  if (metric == "rho") {
    return(cor(gains, benchmark.gains, method = "spearman"))
  }
  if (metric == "r.auto") {
    return(cor(gains[-length(gains)], gains[-1]))
  }
  if (metric == "rho.auto") {
    return(cor(gains[-length(gains)], gains[-1], method = "spearman"))
  }
}
