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
#' Version: \tab 1.1.4 \cr
#' Date: \tab 2018-08-30 \cr
#' License: \tab GPL-3 \cr
#' }
#'
#' See \href{https://cran.r-project.org/package=stocks}{CRAN documentation} for
#' full list of functions and the
#' \href{https://github.com/vandomed/stocks}{GitHub page} for an overview of the 
#' package with some examples.
#'
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#'
#' @references 
#' Eddelbuettel, D. and Francois, R. (2011) Rcpp: Seamless R and C++ 
#' Integration. Journal of Statistical Software, 40(8), 1-18. 
#' \url{http://www.jstatsoft.org/v40/i08/}.
#' 
#' Eddelbuettel, D. (2013) Seamless R and C++ Integration with Rcpp. Springer, 
#' New York. ISBN 978-1-4614-6867-7.
#' 
#' Eddelbuettel, D. and Balamuta, J.J. (2017). Extending R with C++: A Brief 
#' Introduction to Rcpp. PeerJ Preprints 5:e3188v1. 
#' \url{https://doi.org/10.7287/peerj.preprints.3188v1}.
#' 
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
#' @importFrom TTR ROC
#' @importFrom zoo rollapply
#' @useDynLib stocks, .registration=TRUE
#' @name stocks
NULL