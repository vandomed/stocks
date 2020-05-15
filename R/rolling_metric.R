#' Calculate Moving-Window Performance Metrics
#'
#' Mainly a helper function for \code{\link{plot_metrics_overtime}}.
#'
#'
#' @param gains Numeric vector.
#' @param metric Character string.
#' @param width Integer value.
#' @param units.year Integer value.
#' @param benchmark.gains Numeric vector.
#'
#'
#' @return
#' Numeric vector.
#'
#'
#' @export
rolling_metric <- function(gains,
                           metric = "mean",
                           width = 50,
                           units.year = 252,
                           benchmark.gains = NULL) {

  if (metric == "first") {
    return(gains[seq(1, length(gains), width)])
  }
  if (metric == "last") {
    return(gains[seq(width, length(gains), width)])
  }
  if (metric == "cagr") {
    return(convert_gain(roll_prod(gains + 1, width)[-c(1: (width - 1))] - 1, units.in = width, units.out = units.year) * 100)
  }
  if (metric == "mdd") {
    return(rollapply(gains, width, function(x) mdd(gains = x)) * 100)
  }
  if (metric == "mean") {
    return(moving_mean(gains, width) * 100)
  }
  if (metric == "sd") {
    return(roll_sd(gains, width, center = FALSE)[-c(1: (width - 1))] * 100)
  }
  if (metric == "sharpe") {
    return(moving_mean(gains, width) / roll_sd(gains, width)[-c(1: (width - 1))])
  }
  if (metric == "sortino") {
    return(rollapply(gains, width, sortino))
  }
  if (metric == "growth") {
    return((roll_prod(gains + 1, width)[-c(1: (width - 1))] - 1) * 100)
  }
  if (metric == "alpha") {
    return(roll_lm(x = benchmark.gains, y = gains, width = width)$coefficients[-c(1: (width - 1)), 1] * 100)
  }
  if (metric == "alpha.annualized") {
    return(convert_gain(roll_lm(x = benchmark.gains, y = gains, width = width)$coefficients[-c(1: (width - 1)), 1], 1, units.year) * 100)
  }
  if (metric == "beta") {
    return(roll_lm(x = benchmark.gains, y = gains, width = width)$coefficients[-c(1: (width - 1)), 2])
  }
  if (metric == "r.squared") {
    return((roll_lm(x = benchmark.gains, y = gains, width = width)$r.squared[, 1])[-c(1: (width - 1))])
  }
  if (metric == "r") {
    return(roll_cor(x = benchmark.gains, y = gains, width = width)[-c(1: (width - 1))])
  }
  if (metric == "rho") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- cor(gains[locs], benchmark.gains[locs], method = "spearman")
    }
    return(y)
  }
  if (metric == "r.auto") {
    return(roll_cor(x = gains[-length(gains)], y = gains[-1], width = width)[-c(1: (width - 1))])
  }
  if (metric == "rho.auto") {
    return(rollapply(gains, width + 1, function(x) {
      cor(x[-length(x)], x[-1], method = "spearman")
    }))
  }
}
