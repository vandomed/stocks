#' Download Historical Prices
#'
#' Downloads historical prices for specified tickers from Yahoo! Finance, with
#' various options. Relies heavily on the \pkg{quantmod} package.
#'
#'
#' @param tickers Character vector of ticker symbols that Yahoo! Finance
#' recognizes.
#' @param intercepts Numeric vector of values to add to daily gains for each
#' fund.
#' @param slopes Numeric vector of values to multiply daily gains for each fund
#' by. Slopes are multiplied prior to adding intercepts.
#' @param from Date or character string, e.g. \code{"2015-01-15"}.
#' @param to Date or character string, e.g. \code{"2018-12-31"}.
#' @param time.scale Character string. Choices are \code{"daily"},
#' \code{"monthly"}, and \code{"yearly"}.
#' @param preto.days Numeric value. If specified, function returns prices for
#' \code{preto.days} trading days prior to \code{to}. For example, to load the
#' most recent 50 closing prices, leave \code{to} and \code{time.scale} as the
#' defaults and set \code{preto.days = 50}.
#' @param prefrom.days Numeric value. If specified, function returns prices for
#' \code{prefrom.days} trading days prior to \code{from}. Useful when you want
#' to test a trading strategy starting on a particular date, but the strategy
#' requires data leading up to that date (e.g. trailing beta).
#' @param initial Numeric value specifying what value to scale initial prices
#' to.
#' @param mutual.start Logical value for whether to start on the first day of
#' the funds' mutual lifetimes.
#' @param mutual.end Logical value for whether to end on the last day of the
#' funds' mutual lifetimes.
#' @param anchor Logical value for whether to anchor the starting price for each
#' fund to the price of the longest-running fund on that day. Useful for
#' visualizing funds' entire histories while also fairly comparing them over
#' their mutual lifetimes. Only used if \code{mutual.start = FALSE}.
#' @param drop.anyNA Logical value for whether to drop dates on which prices are
#' missing for any of the funds.
#'
#'
#' @return Data frame with closing prices for each fund.
#'
#'
#' @examples
#' \dontrun{
#' # Load prices for Netflix and Amazon over their mutual lifetimes
#' prices <- load_prices(c("NFLX", "AMZN"))
#' }
#'
#'
#' @references
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial
#' Modelling Framework. R package version 0.4-15.
#' \url{https://CRAN.R-project.org/package=quantmod}
#'
#'
#' @export
load_prices <- function(tickers,
                        intercepts = NULL,
                        slopes = NULL,
                        from = "1950-01-01",
                        to = Sys.Date(),
                        time.scale = "daily",
                        preto.days = NULL,
                        prefrom.days = NULL,
                        initial = NULL,
                        mutual.start = FALSE,
                        mutual.end = TRUE,
                        anchor = FALSE,
                        drop.anyNA = FALSE) {

  # Error checking
  if (! is.character(tickers)) {
    stop("The input 'tickers' must be a character vector of ticker symbols")
  }

  # Adjust 'from' date if preto.days or prefrom.days is specified
  from.initial <- from <- as.Date(from)
  to.initial <- to <- as.Date(to) + 1
  if (! is.null(prefrom.days)) {
    from <- from - ifelse(prefrom.days <= 10, 20, ceiling(prefrom.days * 1.65))
  }
  if (! is.null(preto.days)) {
    to <- to + 5
  }

  # Download prices from Yahoo! Finance
  prices.list <- lapply(tickers, function(x) {

    y <- try(getSymbols(Symbols = x, from = from, to = to, auto.assign = FALSE), silent = TRUE)
    if (class(y)[1] == "try-error") {
      message(paste("No available data for", x))
      return(NULL)
    }
    y <- as.data.table(y)
    cbind(data.table(Date = y$index), y[, 7])

  })

  null.tickers <- sapply(prices.list, is.null)
  prices.list[null.tickers] <- NULL
  tickers <- tickers[! null.tickers]
  prices <- as.data.frame(reduce(prices.list, .f = function(x, y) merge(x, y, by = "Date", all = TRUE)))
  names(prices) <- c("Date", tickers)

  # If mutual.start = TRUE, drop rows prior to youngest fund's start date
  if (mutual.start | mutual.end) {
    complete.locs <- which(complete.cases(prices[-1]))
    start.row <- ifelse(mutual.start, data.table::first(complete.locs), 1)
    end.row <- ifelse(mutual.end, data.table::last(complete.locs), nrow(prices))
    prices <- prices[start.row: end.row, , drop = FALSE]
  }

  # If drop.anyNA = TRUE, remove dates where any fund had NA
  if (drop.anyNA) {
    prices <- prices[complete.cases(prices), , drop = FALSE]
  }

  # If preto.days and/or prefrom.days specified, filter accordingly
  if (! is.null(prefrom.days)) {
    last.drop <- which.max(prices$Date >= from.initial) - prefrom.days - 1
    if (last.drop > 0) {
      prices <- prices[-c(1: last.drop), , drop = FALSE]
    }
  }
  if (! is.null(preto.days)) {
    prices <- tail(prices, preto.days + 1)
  }
  # if (! is.null(preto.days)) {
  #   last.keep <- which.max(prices$Date >= to.initial) - preto.days
  #   if (length(last.keep) > 0) {
  #     prices <- prices[1: last.keep, , drop = FALSE]
  #   }
  # }

  # Convert to prices on last day of month/year if requested
  if (time.scale == "monthly") {
    locs <- which(diff(data.table::month(prices$Date)) %in% c(1, -11))
    prices <- prices[locs, , drop = FALSE]
  } else if (time.scale == "yearly") {
    locs <- which(diff(data.table::year(prices$Date)) == 1)
    prices <- prices[locs, , drop = FALSE]
  }

  # If intercepts and slopes specified, convert to gains, scale gains, and
  # convert back to prices
  if ((! is.null(intercepts) & ! all(intercepts == 0)) |
      (! is.null(slopes) & ! all(slopes == 1))) {

    if (is.null(intercepts)) {
      intercepts <- rep(0, length(slopes))
    }
    if (is.null(slopes)) {
      slopes <- rep(1, length(intercepts))
    }

    gains <- prices_gains(prices)
    gains[-1] <- mapply(FUN = function(x, y, z) x * y + z,
                        x = gains[-1],
                        y = slopes,
                        z = intercepts)
    names(gains)[-1] <- ifelse(slopes == 1, tickers, paste(slopes, "x ", tickers, sep = ""))

    # Convert back to prices
    prices <- gains_prices(gains, initial = 1000, date1 = prices$Date[1])

  }

  # Scale prices to same initial value if requested
  if (! is.null(initial)) {
    if (mutual.start) {
      prices[-1] <- sapply(prices[-1], function(x) x / x[1] * initial)
    } else {
      prices[-1] <- sapply(prices[-1], function(x) x / x[which.max(! is.na(x))] * initial)
    }
  }

  # Anchor prices to oldest fund if requested
  if (! mutual.start & anchor) {
    earliest.ticker <- tickers[which.max(! is.na(prices[1, -1, drop = FALSE]))]
    prices[-1] <- sapply(prices[-1], function(x) {
      loc.start <- which.max(! is.na(x))
      x / x[loc.start] * prices[loc.start, earliest.ticker]
    })
  }

  # Output message indicating date range and return prices
  message(paste("Results span ", first(prices$Date), " to " , last(prices$Date), sep = ""))
  return(prices)

}
