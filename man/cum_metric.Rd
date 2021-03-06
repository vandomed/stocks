% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cum_metric.R
\name{cum_metric}
\alias{cum_metric}
\title{Calculate Cumulative Performance Metrics}
\usage{
cum_metric(gains, metric = "mean", units.year = 252, benchmark.gains = NULL)
}
\arguments{
\item{gains}{Numeric vector.}

\item{metric}{Character string.}

\item{units.year}{Integer value.}

\item{benchmark.gains}{Numeric vector.}
}
\value{
Numeric vector.
}
\description{
Mainly a helper function for \code{\link{plot_metrics_overtime}}. Work in
progress.
}
