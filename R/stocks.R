#' Stock Market Analysis
#'
#' Functions for analyzing stocks or other investments. Main features are
#' loading and aligning historical data for ticker symbols, calculating
#' performance metrics for individual funds or portfolios (e.g. compound
#' annualized growth rate, maximum drawdown, Sharpe/Sortino ratio), and creating
#' graphs. C++ code is used where possible to improve processing speed.
#'
#' \tabular{ll}{
#' Package: \tab stocks \cr
#' Type: \tab Package \cr
#' Version: \tab 1.1.2 \cr
#' Date: \tab 2018-02-20 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See \href{https://cran.r-project.org/package=stocks}{CRAN documentation} for
#' full list of functions.
#'
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#'
#' @references Acknowledgment: This material is based upon work supported by the
#' National Science Foundation Graduate Research Fellowship under Grant No.
#' DGE-0940903.
#'
#' @docType package
#'
#'
#' @import curl
#' @importFrom dvmisc inside list_override
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