#' Get Units for Performance Metric
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
metric_units <- function(metric) {
  ifelse(metric %in% c("mean", "sd", "cagr", "mdd", "allocation") | grepl("growth|alpha", metric), "%", "")
}
