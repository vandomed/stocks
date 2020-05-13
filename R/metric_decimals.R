#' Get Number of Decimals for Performance Metric
#'
#' Mainly a helper function.
#'
#'
#' @param metric Character string.
#'
#'
#' @return
#' Character string.
#'
#'
#' @export
metric_decimals <- function(metric) {

  unlist(sapply(metric, function(x) {

    if (x == "cagr") return(1)
    if (x == "mdd") return(1)
    if (x == "mean") return(3)
    if (x == "sd") return (2)
    if (x == "sharpe") return (3)
    if (x == "sortino") return(3)
    if (grepl("growth", x, fixed = TRUE)) return(1)
    if (x == "alpha") return(3)
    if (x == "alpha.annualized") return(1)
    if (x == "beta") return(2)
    if (x == "r.squared") return(2)
    if (x == "r") return(2)
    if (x == "rho") return(2)
    if (x == "r.auto") return(2)
    if (x == "rho.auto") return(2)
    if (x == "allocation") return(0)
    return(2)

  }))

}
