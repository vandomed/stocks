#' Get Units for Performance Metric
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
metric_units <- function(metric) {
  ifelse(metric %in% c("cagr", "mdd", "mean", "sd", "allocation") | grepl("growth|alpha", metric), "%", "")
}
