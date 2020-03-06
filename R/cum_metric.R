#' Calculate Cumulative Performance Metrics
#'
#' Mainly a helper function for \code{\link{plot_metrics_overtime}}. Work in
#' progress.
#'
#'
#' @param gains Numeric vector.
#' @param metric Character string.
#' @param units.year Integer value.
#' @param benchmark.gains Numeric vector.
#'
#'
#' @return
#' Numeric vector.
#'
#'
#' @export
cum_metric <- function(gains,
                       metric = "mean",
                       units.year = 252,
                       benchmark.gains = NULL) {

  if (! metric %in% c("mean", "growth", "cagr")) {
    stop("This function is a work in progress, and currently the only supported metrics are 'mean', 'growth', and 'cagr'")
  }

  if (metric == "mean") {
    return(cummean(gains) * 100)
  }
  # if (metric == "sd") {
  #   return(rollapply(gains, width, sd) * 100)
  # }
  if (metric == "growth") {
    return((cumprod(gains + 1) - 1) * 100)
  }
  if (metric == "cagr") {
    return(convert_gain(cumprod(gains + 1) - 1, units.in = 1: length(gains), units.out = units.year) * 100)
  }
  stop("Currently the only supported metrics are 'mean', 'growth', and 'cagr'.")
  # if (metric == "mdd") {
  #   return(rollapply(gains, width, function(x) mdd(gains = x)) * 100)
  # }
  # if (metric == "sharpe") {
  #   return(rollapply(gains, width, sharpe))
  # }
  # if (metric == "sortino") {
  #   return(rollapply(gains, width, sortino))
  # }
  # if (metric == "alpha") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <- lm(gains[locs] ~ benchmark.gains[locs])$coef[1] * 100
  #   }
  #   return(y)
  # }
  # if (metric == "alpha.annualized") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <-
  #       convert_gain(lm(gains[locs] ~ benchmark.gains[locs])$coef[1], 1, units.year) * 100
  #   }
  #   return(y)
  # }
  # if (metric == "beta") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <- lm(gains[locs] ~ benchmark.gains[locs])$coef[2]
  #   }
  #   return(y)
  # }
  # if (metric == "r.squared") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <- summary(lm(gains[locs] ~ benchmark.gains[locs]))$r.squared
  #   }
  #   return(y)
  # }
  # if (metric == "pearson") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <- cor(gains[locs], benchmark.gains[locs])
  #   }
  #   return(y)
  # }
  # if (metric == "spearman") {
  #   y <- c()
  #   for (ii in (width: length(gains))) {
  #     locs <- (ii - width + 1): ii
  #     y[(ii - width + 1)] <- cor(gains[locs], benchmark.gains[locs], method = "spearman")
  #   }
  #   return(y)
  # }
  # if (metric == "auto.pearson") {
  #   return(rollapply(gains, width + 1, function(x) {
  #     cor(x[-length(x)], x[-1])
  #   }))
  # }
  # if (metric == "auto.spearman") {
  #   return(rollapply(gains, width + 1, function(x) {
  #     cor(x[-length(x)], x[-1], method = "spearman")
  #   }))
  # }
}

