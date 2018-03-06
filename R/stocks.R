#' Stock Market Analysis
#'
#' Functions for analyzing stocks or other investments. Main features are
#' loading and aligning historical data for ticker symbols, calculating
#' performance metrics for individual funds or portfolios (e.g. annualized 
#' growth, maximum drawdown, Sharpe/Sortino ratio), and creating graphs. C++ 
#' code is used to improve processing speed where possible.
#'
#' \tabular{ll}{
#' Package: \tab stocks \cr
#' Type: \tab Package \cr
#' Version: \tab 1.1.2 \cr
#' Date: \tab 2018-03-05 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See \href{https://cran.r-project.org/package=stocks}{CRAN documentation} for
#' full list of functions.
#'
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#'
#' @references 
#' Ryan, J.A. and Ulrich, J.M. (2017) quantmod: Quantitative Financial Modelling 
#' Framework. R package version 0.4-12, 
#' \url{https://CRAN.R-project.org/package=quantmod}.
#' 
#' Acknowledgment: This material is based upon work supported by the
#' National Science Foundation Graduate Research Fellowship under Grant No.
#' DGE-0940903.
#'
#' @docType package
#'
#'
#' @importFrom dvmisc inside list_override mean_n sd_n
#' @import graphics
#' @import grDevices
#' @importFrom Hmisc capitalize
#' @import lubridate
#' @import methods
#' @import quantmod
#' @import RColorBrewer
#' @importFrom Rcpp evalCpp
#' @importFrom rbenchmark benchmark
#' @import stats
#' @importFrom zoo rollapply
#'
#'
#' @useDynLib stocks, .registration=TRUE
#' @name stocks
NULL