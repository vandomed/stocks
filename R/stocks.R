#' Stock Market Analysis
#'
#' Functions for analyzing stocks or other investments. Main features are
#' loading and aligning historical data for ticker symbols, calculating
#' performance metrics for individual funds or portfolios (e.g. annualized
#' growth, maximum drawdown, Sharpe/Sortino ratio), and creating graphs.
#'
#' \tabular{ll}{
#' Package: \tab stocks \cr
#' Type: \tab Package \cr
#' Version: \tab 2.0.0 \cr
#' Date: \tab 2019-12-04 \cr
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
#' \url{http://www.jstatsoft.org/v40/i08/}
#'
#' Eddelbuettel, D. (2013) Seamless R and C++ Integration with Rcpp. Springer,
#' New York. ISBN 978-1-4614-6867-7.
#'
#' Eddelbuettel, D. and Balamuta, J.J. (2017). Extending R with C++: A Brief
#' Introduction to Rcpp. PeerJ Preprints 5:e3188v1.
#' \url{https://doi.org/10.7287/peerj.preprints.3188v1}
#'
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial
#' Modelling Framework. R package version 0.4-15.
#' \url{https://CRAN.R-project.org/package=quantmod}
#'
#'
#' @docType package
#'
#'
#' @importFrom accelerometry movingaves
#' @importFrom data.table as.data.table data.table first last melt
#' @importFrom dplyr %>% bind_rows group_by mutate mutate_at row_number slice tibble ungroup
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices hcl
#' @importFrom lubridate month year
#' @import methods
#' @importFrom plotly ggplotly
#' @importFrom purrr reduce
#' @import quantmod
#' @importFrom Rcpp evalCpp
#' @importFrom rbenchmark benchmark
#' @importFrom rvest html_node html_nodes html_table
#' @importFrom scales comma hue_pal
#' @import stats
#' @importFrom tidyr as_tibble pivot_longer
#' @importFrom TTR ROC
#' @importFrom utils head tail
#' @importFrom xml2 read_html
#' @importFrom zoo rollapply
#' @useDynLib stocks, .registration=TRUE
#' @name stocks
NULL
utils::globalVariables(
  c(".", "Allocation (%)", "Allocation 1 (%)", "Allocation 2 (%)",
    "Allocation 3 (%)", "Balance", "Date", "Fund", "Gain", "Label",
    "metric.info", "n", ".N", "Pair", "Period", "Set",
    ".SD", "sp500.dates", "text", "tooltip", "Trio")
)
