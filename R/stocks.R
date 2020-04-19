#' Stock Market Analysis
#'
#' Functions for analyzing and visualizing stock market data. Main features are
#' loading and aligning historical data, calculating performance metrics for
#' individual funds or portfolios (e.g. annualized growth, maximum drawdown,
#' Sharpe/Sortino ratio), and creating graphs.
#'
#' \tabular{ll}{
#' Package: \tab stocks \cr
#' Type: \tab Package \cr
#' Version: \tab 2.0.0 \cr
#' Date: \tab 2020-04-19 \cr
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
#' @importFrom fastmatch ctapply
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices hcl
#' @importFrom lubridate month year
#' @import methods
#' @importFrom plotly ggplotly style
#' @importFrom purrr reduce
#' @import quantmod
#' @importFrom Rcpp evalCpp
#' @importFrom Rfast lmfit
#' @importFrom rbenchmark benchmark
#' @importFrom roll roll_cor roll_lm roll_prod roll_sd
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
    "Allocation 3 (%)", "Balance ($)", "Date", "End date", "Fund", "Gain",
    "Gain (%)", "Label", "metric.info", "n", ".N", "Pair", "Period", "Set",
    ".SD", "sp500.dates", "Start date", "text", "tooltip", "Trio")
)
