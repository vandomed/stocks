#' Download and Align Historical Prices for a Set of Tickers
#' 
#' Downloads and aligns historical prices for specified tickers from Yahoo! 
#' Finance, using the \pkg{quantmod} package.
#' 
#' @inherit load_gains details
#' 
#' 
#' @inheritParams load_gains
#' @param initial Numeric value specifying what value to scale initial prices 
#' to.
#' 
#' 
#' @return Numeric matrix.
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples 
#' \dontrun{
#' # Load prices for Netflix and Amazon over their mutual lifetimes
#' prices <- load_prices(c("NFLX", "AMZN"))
#' }
#' 
#' 
#' @inherit load_gains references
#' 
#' 
#' @export
load_prices <- function(tickers, intercepts = NULL, slopes = NULL, 
                        from = "1950-01-01", to = Sys.Date(),
                        time.scale = "daily",
                        preto.days = NULL, prefrom.days = NULL,
                        initial = NULL,
                        earliest = FALSE, latest = FALSE) {
  
  # Adjust from date if preto.days or prefrom.days are specified
  from.initial <- from <- as.Date(from)
  to <- as.Date(to) + 1
  if (! is.null(preto.days)) {
    from <- to - ifelse(preto.days <= 10, 20, ceiling(preto.days * 1.65))
  }
  if ( ! is.null(prefrom.days)) {
    from <- from - ifelse(prefrom.days <= 10, 20, ceiling(prefrom.days * 1.65))
  }
  
  # Download prices from Yahoo! Finance using 'quantmod' package
  prices <- list()
  for (ii in 1: length(tickers)) {
    prices.fund <- try(getSymbols(Symbols = tickers[ii], from = from, to = to,
                                  auto.assign = FALSE), silent = TRUE)
    if (class(prices.fund)[1] == "try-error") {
      prices[[ii]] <- NULL
    } else {
      prices.fund <- as.matrix(prices.fund)
      locs.remove <- which(prices.fund[, 4] %in% c(0, NA))
      if (length(locs.remove) > 0) {
        prices.fund <- prices.fund[-locs.remove, , drop = F]
      }
      prices[[ii]] <- prices.fund
      if (! earliest) {
        from <- max(as.Date(from),
                    as.Date(rownames(prices.fund[1, , drop = F])))
      }
    }
  }
  
  # Drop tickers that could not be loaded
  locs <- sapply(prices, function(x) ! is.null(x))
  if (! all(locs)) {
    tickers <- tickers[locs]
    prices <- prices[locs]
    intercepts <- intercepts[locs]
    slopes <- slopes[locs]
  }
  
  # If more than 1 fund, align prices
  if (length(tickers) > 1) {
    
    # Align start dates
    start.dates <- as.Date(unlist(lapply(prices, function(x)
      rownames(x[1, , drop = F]))))
    if (earliest) {
      earliest.startdate <- min(start.dates)
      locs.earliest <- which(start.dates == earliest.startdate)
      tickers <- tickers[locs.earliest]
      start.dates <- start.dates[locs.earliest]
      prices <- prices[locs.earliest]
    } else {
      if (length(unique(start.dates)) > 1) {
        latest.startdate <- max(start.dates)
        for (ii in 1: length(tickers)) {
          if (start.dates[ii] != latest.startdate) {
            prices.fund <- prices[[ii]]
            dates.fund <- as.Date(rownames(prices.fund))
            loc.start <- which(dates.fund == latest.startdate)
            prices.fund <- prices.fund[loc.start: nrow(prices.fund), ]
            prices[[ii]] <- prices.fund
          }
        }
      }
    }
    
    # Align end dates
    end.dates <- as.Date(unlist(lapply(prices, function(x)
      rownames(x[nrow(x), , drop = F]))))
    if (latest) {
      latest.enddate <- max(end.dates)
      locs.latest <- which(end.dates == latest.enddate)
      tickers <- tickers[locs.latest]
      end.dates <- end.dates[locs.latest]
      prices <- prices[locs.latest]
    } else {
      if (length(unique(end.dates)) > 1) {
        earliest.enddate <- min(end.dates)
        for (ii in 1: length(tickers)) {
          if (end.dates[ii] != earliest.enddate) {
            prices.fund <- prices[[ii]]
            dates.fund <- as.Date(rownames(prices.fund))
            loc.end <- which(dates.fund == earliest.enddate)
            prices.fund <- prices.fund[1: loc.end, ]
            prices[[ii]] <- prices.fund
          }
        }
      }
    }
    
    # Go through and remove any dates that don't match the others
    ii <- 2
    while (ii <= length(tickers)) {
      
      # Get price data for 1st and iith fund
      prices.fund1 <- prices[[1]]
      dates.fund1 <- as.Date(rownames(prices.fund1))
      
      prices.fund2 <- prices[[ii]]
      dates.fund2 <- as.Date(rownames(prices.fund2))
      
      # As long as at least 1 date doesn't match up, remove the unmatched date
      while (! suppressWarnings(all(dates.fund1 == dates.fund2))) {
        
        loc <- suppressWarnings(which(dates.fund1 != dates.fund2))[1]
        if (dates.fund1[loc] < dates.fund2[loc]) {
          message(paste("Dropped", dates.fund1[loc], "from", tickers[1],
                        sep = " "))
          prices.fund1 <- prices.fund1[-loc, ]
          dates.fund1 <- dates.fund1[-loc]
          prices[[1]] <- prices.fund1
        } else {
          message(paste("Dropped", dates.fund2[loc], "from", tickers[ii],
                        sep = " "))
          prices.fund2 <- prices.fund2[-loc, ]
          dates.fund2 <- dates.fund2[-loc]
          prices[[ii]] <- prices.fund2
        }
        ii <- 1
        
      }
      ii <- ii + 1
      
    }
    
  }
  
  # Get dates
  dates <- as.Date(rownames(prices[[1]]))
  length.dates <- length(dates)
  
  # If preto.days and/or prefrom.days specified, get just the date range of
  # interest
  if (! is.null(prefrom.days) & ! is.null(preto.days)) {
    prices <- lapply(prices, function(x)
      x[(length.dates - prefrom.days - preto.days): length.dates, ])
  } else if (! is.null(prefrom.days) & is.null(preto.days)) {
    loc.from <- which(dates == from.initial)
    prices <- lapply(prices, function(x)
      x[(loc.from - prefrom.days): length.dates, ])
  } else if (is.null(prefrom.days) & ! is.null(preto.days)) {
    prices <- lapply(prices, function(x)
      x[(length.dates - preto.days): length.dates, ])
  }
  dates <- as.Date(rownames(prices[[1]]))
  
  # Convert to prices on last day of month/year if requested
  if (time.scale == "monthly") {
    locs <- which(diff(month(dates)) %in% c(1, -11))
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
    # dates <- sapply(dates, function(x)
    #   paste(unlist(strsplit(as.character(x), "-"))[-3], collapse = "-"))
  } else if (time.scale == "yearly") {
    locs <- which(diff(year(dates)) == 1)
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
  }
  
  # Create matrix of closing prices
  closing.prices <- matrix(unlist(lapply(prices, function(x)
    x[, 6])), byrow = F, ncol = length(tickers))
  colnames(closing.prices) <- tickers
  rownames(closing.prices) <- as.character(dates)
  
  # If intercepts and slopes specified, convert to gains, scale gains, and
  # convert back to prices
  if ((!is.null(intercepts) & !all(intercepts == 0)) |
      (!is.null(slopes) & !all(slopes == 1))) {
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- rep(0, length(tickers))
    }
    if (is.null(slopes)) {
      slopes <- rep(1, length(tickers))
    }
    
    # Scale each column of closing prices
    for (ii in 1: length(tickers)) {
      if (intercepts[ii] != 0 | slopes[ii] != 1) {
        closing.prices[, ii] <- 
          gains_prices(gains = intercepts[ii] + slopes[ii] * 
                         pchanges(closing.prices[, ii]), 
                       initial = closing.prices[1, ii])
        if (slopes[ii] != 1) {
          tickers[ii] <- paste(slopes[ii], "x ", tickers[ii], sep = "")
        }
      }
    }
    colnames(closing.prices) <- tickers
    
  }
  
  # Scale prices to same initial value if requested
  if (!is.null(initial)) {
    closing.prices <- apply(closing.prices, 2, function(x) x / x[1] * initial)
  }
  
  # Output message indicating date range
  message(paste("Results are for ", dates[1], " to " , dates[length(dates)],
                sep = ""))
  
  # Return closing prices
  return(closing.prices)
  
}