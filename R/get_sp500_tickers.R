#' Get S&P 500 Ticker Symbols as on a Particular Date
#'
#' Scrapes ticker symbols from the Wikipedia Revision history
#' (\url{https://en.wikipedia.org/wiki/List_of_S%26P_500_companies}). Of course,
#' the data may be imperfect.
#'
#'
#' @param date Date (or character vector that can be coerced).
#'
#'
#' @return
#' Character vector.
#'
#'
#' @examples
#' \dontrun{
#' # S&P 500 tickers as of today
#' head(get_sp500_tickers())
#'
#' # S&P 500 tickers at the beginning of 2019
#' head(get_sp500_tickers("2019-01-01"))
#' }
#'
#'
#' @export
get_sp500_tickers <- function(date = Sys.Date()) {

  # Get most recent date on or before 'date'
  df <- sp500.dates %>%
    dplyr::filter(Date <= as.Date(date)) %>%
    dplyr::slice(1)

  # Get ticker symbols
  tickers <- (df$url %>%
    read_html() %>%
    html_node("table") %>%
    html_table())[[1]]

  # Replace .'s with -'s for when we download data from Yahoo! Finance
  gsub("[.]", "-", x = tickers)

}
