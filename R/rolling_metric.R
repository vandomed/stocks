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
  if (metric == "mean") {
    return(movingaves(gains, window = width) * 100)
  }
  if (metric == "sd") {
    return(rollapply(gains, width, sd) * 100)
  } 
  if (metric == "growth") {
    return(rollapply(gains, width, gains_rate) * 100)
  }
  if (metric == "cagr") {
    return(rollapply(gains, width, function(x) gains_rate(x, units.year)) * 100)
  }
  if (metric == "mdd") {
    return(rollapply(gains, width, function(x) mdd(gains = x)) * 100)
  } 
  if (metric == "sharpe") {
    return(rollapply(gains, width, sharpe))
  } 
  if (metric == "sortino") {
    return(rollapply(gains, width, sortino))
  } 
  if (metric == "alpha") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- lm(gains[locs] ~ benchmark.gains[locs])$coef[1] * 100
    }
    return(y)
  }
  if (metric == "alpha.annualized") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- 
        convert_gain(lm(gains[locs] ~ benchmark.gains[locs])$coef[1], 1, units.year) * 100
    }
    return(y)
  }
  if (metric == "beta") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- lm(gains[locs] ~ benchmark.gains[locs])$coef[2]
    }
    return(y)
  }
  if (metric == "r.squared") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- summary(lm(gains[locs] ~ benchmark.gains[locs]))$r.squared
    }
    return(y)
  }
  if (metric == "pearson") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- cor(gains[locs], benchmark.gains[locs])
    }
    return(y)
  }
  if (metric == "spearman") {
    y <- c()
    for (ii in (width: length(gains))) {
      locs <- (ii - width + 1): ii
      y[(ii - width + 1)] <- cor(gains[locs], benchmark.gains[locs], method = "spearman")
    }
    return(y)
  } 
  if (metric == "auto.pearson") {
    return(rollapply(gains, width + 1, function(x) {
      cor(x[-length(x)], x[-1])
    }))
  }
  if (metric == "auto.spearman") {
    return(rollapply(gains, width + 1, function(x) {
      cor(x[-length(x)], x[-1], method = "spearman")
    }))
  }
}
