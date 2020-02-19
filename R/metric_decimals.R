#' Get Number of Decimals for Performance Metric
#'
#' For internal use only.
#'
#'
#' @param metric Character string.
#'
#'
#' @return
#' Character string.
#'
#'
metric_decimals <- function(metric) {

  unlist(sapply(metric, function(x) {
    if (x == "mean") return(2)
    if (x == "sd") return (2)
    if (grepl("growth", x, fixed = TRUE)) return(1)
    if (x == "cagr") return(1)
    if (x == "mdd") return(1)
    if (x == "sharpe") return (3)
    if (x == "sortino") return(3)
    if (x == "alpha") return(3)
    if (x == "alpha.annualized") return(1)
    if (x == "beta") return(2)
    if (x == "r.squared") return(2)
    if (x == "pearson") return(2)
    if (x == "spearman") return(2)
    if (x == "auto.pearson") return(2)
    if (x == "auto.spearman") return(2)
    if (x == "allocation") return(0)
  }))

}
