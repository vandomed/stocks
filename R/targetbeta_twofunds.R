#' Backtest a Two-Fund Strategy that Targets a Certain Beta
#' 
#' Implements a two-fund strategy where allocations to each fund are adjusted to 
#' maintain some user-specified portfolio beta. For example, you could back-test 
#' a zero-beta (i.e. market neutral) UPRO/VBLTX strategy using this function.
#' 
#' The general implementation is as follows. Beta for each of the two funds is 
#' estimated based on the first \code{window.units} gains. Initial allocations 
#' are selected to achieve portfolio beta of \code{target.beta}. If that is not 
#' possible - for example, if \code{target.beta = 0} and both funds have 
#' positive beta - then the action taken depends on what method is selected 
#' through the \code{failure.method} input (details below).
#' 
#' Assuming the target beta is attainable, the function moves over 1 day, and 
#' applies each fund's gains for that day. It then re-calculates each fund's 
#' beta based on the \code{window.units}-width interval, and determines the 
#' effective portfolio beta based on fund allocations and betas. If the 
#' effective beta is outside of \code{[target.beta - tol, target.beta + tol]}, a 
#' rebalancing trade is triggered. As before, if the target beta cannot be 
#' achieved, certain actions are taken depending on the selected method.
#' 
#' When outside of a trade because the target beta could not be achieved, the 
#' function attempts to rebalance each time it shifts over to a new day, 
#' regardless of the effective portfolio beta.
#' 
#' When \code{failure.method = "cash"}, the entire portfolio balance is 
#' allocated to cash when the target beta cannot be achieved. 
#' 
#' When \code{failure.method = "fund1"} (or \code{"fund2"}), the entire 
#' portfolio balance is allocated to the first (or second) fund when the target 
#' beta cannot be achieved.
#' 
#' When \code{failure.method = "fund1.maxall"} (or \code{"fund2.maxall"}), when 
#' the target beta cannot be achieved, fund 1 (or fund 2) is combined with cash, 
#' with the fund 1 (fund 2) allocation as high as possible while staying within 
#' \code{maxall.tol} of \code{target.beta}.
#' 
#' When \code{failure.method = "inverse1"} (or \code{"inverse2"}), an inverse 
#' version of the first (or second) fund is used when the target beta cannot be 
#' achieved. In many cases where the target beta cannot be achieved with the two 
#' funds, it can be achieved with an inverse version of one and the other. If 
#' the target beta still cannot be achieved, the entire portfolio balance is 
#' allocated to cash.
#' 
#' When \code{failure.method = "closer"}, the entire portfolio balance is 
#' allocated to whichever fund has a beta closer to \code{target.beta}.
#' 
#' 
#' @inheritParams onemetric_graph
#' @inheritParams twofunds_graph
#' @inheritParams load_gains
#' @inheritParams load_prices
#' 
#' @param tickers Character vector specifying 2 ticker symbols that Yahoo! 
#' Finance recognizes, if you want to download data on the fly.
#' 
#' @param benchmark.ticker Character string specifying ticker symbol for 
#' benchmark index for calculating beta. If unspecified, the first fund in 
#' \code{tickers} is used as the benchmark.
#'
#' @param benchmark.gains Numeric vector of gains for the benchmark index for 
#' calculating beta. If unspecified, the first fund in \code{tickers.gains} is 
#' used as the benchmark.
#' 
#' @param target.beta Numeric value.
#' 
#' @param tol Numeric value specifying how far the effective portfolio beta has 
#' to deviate from \code{target.beta} to trigger a rebalancing trade.
#' 
#' @param window.units Numeric value specifying the width of the trailing moving 
#' window used to estimate each fund's beta.
#' 
#' @param failure.method Character string or vector specifying method(s) to use 
#' when fund betas are such that the target portfolio beta cannot be achieved. 
#' Choices are \code{"cash"}, \code{"fund1"}, \code{"fund2"}, 
#' \code{"fund1.maxall"}, \code{"fund2.maxall"}, \code{"inverse1"}, 
#' \code{"inverse2"}, and \code{"closer"}. See Details.
#' 
#' @param maxall.tol Numeric value specifying tolerance to use when implementing 
#' the \code{"fund1.maxall"} or \code{"fund2.maxall"} failure method. To 
#' illustrate, if \code{target.beta = 0}, fund 1 has a current beta of 1, fund 2 
#' has a current beta of 0.25, \code{failure.method = "fund2.maxall"}, and 
#' \code{maxall.tol = 0.1}, a trade will be triggered that results in 40\% fund 
#' 2 and 60\% cash. The portfolio beta is \code{0.4 * 0.25 = 0.1}. The reason 
#' you might want \code{maxall.tol} to be less than \code{tol} is to avoid 
#' frequently triggering another trade on the very next day, as fund 2's beta 
#' changes a little and moves the portfolio beta outside of 
#' \code{[target.beta - tol, target.beta + tol]}.
#'
#' 
#' @return
#' For each method, a 4-element list containing: 
#' \enumerate{
#' \item Numeric matrix named \code{fund.balances} giving fund balances over 
#' time. 
#' \item Numeric matrix named \code{fund.betas} giving fund betas over time. 
#' \item Numeric vector named \code{effective.betas} giving effective portfolio 
#' beta over time. 
#' \item Numeric value named \code{trades} giving the total number of trades 
#' executed.
#' }
#' 
#' 
#' @inherit ticker_dates references
#' 
#' 
#' @examples
#' \dontrun{
#' # Backtest zero-beta UPRO/VBLTX strategy
#' beta0 <- targetbeta_twofunds(tickers = c("UPRO", "VBLTX"), target.beta = 0)
#' plot(beta0$fund.balances[, "Portfolio"])
#' }
#'
#' @export
targetbeta_twofunds <- function(tickers = NULL,
                                intercepts = NULL, slopes = NULL, ...,
                                benchmark.ticker = NULL,
                                reference.tickers = NULL,
                                tickers.gains = NULL,
                                benchmark.gains = NULL,
                                reference.gains = NULL,
                                target.beta = 0,
                                tol = 0.15,
                                window.units = 50,
                                failure.method = "closer",
                                maxall.tol = tol - 0.05,
                                initial = 10000) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (! is.null(tickers)) {
    
    # Get number of tickers
    n.tickers <- length(tickers)
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- rep(0, n.tickers)
    }
    if (is.null(slopes)) {
      slopes <- rep(1, n.tickers)
    }
    
    # Create vector of "extra" tickers comprised of benchmark and reference
    # tickers
    extra.tickers <- unique(c(benchmark.ticker, reference.tickers))
    n.extra <- length(extra.tickers)
    
    # Calculate gains matrix
    tickers.vec <- c(tickers, extra.tickers)
    intercepts.vec <- c(intercepts, rep(0, n.extra))
    slopes.vec <- c(slopes, rep(1, n.extra))
    gains <- load_gains(tickers = tickers.vec, intercepts = intercepts.vec,
                        slopes = slopes.vec, ...)
    
    # Update ticker names to show intercept/slope
    tickers <- colnames(gains)[1: n.tickers]
    
    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: n.tickers, drop = F]
    extra.gains <- gains[, -c(1: n.tickers), drop = F]
    if (! is.null(benchmark.ticker)) {
      benchmark.gains <- extra.gains[, benchmark.ticker, drop = F]
    }
    if (! is.null(reference.tickers)) {
      reference.gains <- extra.gains[, reference.tickers, drop = F]
    }
    
  } else {
    
    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    n.tickers <- length(tickers)
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: n.tickers, sep = "")
    }
    
    # Convert reference.gains to matrix if necessary, and figure out
    # reference.tickers
    if (is.vector(reference.gains)) {
      reference.gains <- matrix(reference.gains, ncol = 1)
      reference.tickers <- "REF"
    } else if (is.matrix(reference.gains)) {
      reference.tickers <- colnames(reference.gains)
      if (is.null(reference.tickers)) {
        reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
      }
    }
    
  }
  
  # Calculate acceptable interval for effective portfolio beta
  beta.range <- c(target.beta - tol, target.beta + tol)
  
  # Get dates for row names of various results
  dates <- rownames(tickers.gains)[-c(1: (window.units - 1))]
  
  # If benchmark.gains is not specified, use 1st column of gains as benchmark
  if (is.null(benchmark.gains)) {
    benchmark.gains <- tickers.gains[, 1]
    col1.benchmark <- TRUE
  } else {
    col1.benchmark <- FALSE
  }
  
  # Extract gains for fund 1 and fund 2
  fund1.gains <- tickers.gains[, 1]
  fund2.gains <- tickers.gains[, 2]
  
  # Calculate betas for both funds over entire time period
  if (col1.benchmark) {
    fund1.betas <- rep(1, length(fund1.gains) - window.units + 1)
  } else {
    fund1.betas <- rollapply(cbind(benchmark.gains, fund1.gains),
                             width = window.units, by.column = FALSE,
                             FUN = function(x) lm(x[, 2] ~ x[, 1])$coef[2])
  }
  fund2.betas <- rollapply(cbind(benchmark.gains, fund2.gains),
                           width = window.units, by.column = FALSE,
                           FUN = function(x) lm(x[, 2] ~ x[, 1])$coef[2])
  fund.betas <- matrix(c(fund1.betas, fund2.betas), ncol = 2,
                       dimnames = list(NULL, colnames(tickers.gains)))
  
  # Initialize results.list list
  results.list <- list()
  
  # Implement "cash" failure method if requested
  if ("cash" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1, fund 2, and cash
    if (inside(fund1.all, c(0, 1))) {
      fund2.all <- 1 - fund1.all
      cash.all <- 0
    } else {
      fund1.all <- 0
      fund2.all <- 0
      cash.all <- 1
    }
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    cash.bal <- initial * cash.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 4,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates, c(colnames(tickers.gains),
                                                     "Cash", "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal + cash.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (cash.bal > 0) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, continue to hold 100% cash.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          cash.bal <- 0
          
        } else {
          
          fund1.all <- 0
          fund2.all <- 0
          cash.all <- 1
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to 100% cash.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            cash.bal <- 0
            
          } else {
            
            trades <- trades + 1
            fund1.all <- 0
            fund2.all <- 0
            cash.all <- 1
            fund1.bal <- 0
            fund2.bal <- 0
            cash.bal <- port.bal
            
          }
          
        }
        
      }
      
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, 
              apply(reference.gains, 2, function(x) 
                gains_prices(gains = x[(window.units + 1): length(x)], 
                             initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.cash <- list(fund.balances = fund.balances,
                                      fund.betas = fund.betas,
                                      effective.betas = effective.betas,
                                      trades = trades)
    
  }
  
  # Implement "fund1" failure method if requested
  if ("fund1" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1 and fund 2
    if (! inside(fund1.all, c(0, 1))) {
      fund1.all <- 1
    }
    fund2.all <- 1 - fund1.all
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 3,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates, c(colnames(tickers.gains),
                                                     "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (fund1.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, continue to hold 100% fund 1.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          
        } else {
          
          fund1.all <- 1
          fund2.all <- 0
          fund1.bal <- port.bal
          fund2.bal <- 0
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to 100% fund 1.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            
          } else {
            
            trades <- trades + 1
            fund1.all <- 1
            fund2.all <- 0
            fund1.bal <- port.bal
            fund2.bal <- 0
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, 
              apply(reference.gains, 2, function(x) 
                gains_prices(gains = x[(window.units + 1): length(x)], 
                             initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.fund1 <- list(fund.balances = fund.balances,
                                       fund.betas = fund.betas,
                                       effective.betas = effective.betas,
                                       trades = trades)
    
  }
  
  # Implement "fund2" failure method if requested
  if ("fund2" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1 and fund 2
    if (! inside(fund1.all, c(0, 1))) {
      fund1.all <- 0
    }
    fund2.all <- 1 - fund1.all
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 3,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates, c(colnames(tickers.gains),
                                                     "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocations for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (fund2.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, continue to hold 100% fund 2.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          
        } else {
          
          fund1.all <- 0
          fund2.all <- 1
          fund1.bal <- 0
          fund2.bal <- port.bal
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to 100% fund 2.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            
          } else {
            
            trades <- trades + 1
            fund1.all <- 0
            fund2.all <- 1
            fund1.bal <- 0
            fund2.bal <- port.bal
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.fund2 <- list(fund.balances = fund.balances,
                                       fund.betas = fund.betas,
                                       effective.betas = effective.betas,
                                       trades = trades)
    
  }
  
  # Implement "fund1.maxall" failure method if requested
  if ("fund1.maxall" %in% failure.method) {
    
    # Calculate interval for starting beta when maximizing allocation to fund 1
    maxall.beta.range <- c(target.beta - maxall.tol, target.beta + maxall.tol)
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1 and fund 2
    if (! inside(fund1.all, c(0, 1))) {
      if (inside(fund1.beta, maxall.beta.range)) {
        fund1.all <- 1
      } else {
        fund1.alls <- seq(0, 1, 0.001)
        port.betas <- fund1.alls * fund1.beta
        fund1.all <-
          fund1.alls[which.max(fund1.alls[inside(port.betas,
                                                 maxall.beta.range)])]
      }
      fund2.all <- 0
      cash.all <- 1 - fund1.all
    } else {
      fund2.all <- 1 - fund1.all
      cash.all <- 0
    }
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    cash.bal <- initial * cash.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 4,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates, c(colnames(tickers.gains),
                                                     "Cash", "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal + cash.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (cash.bal > 0 | fund1.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if effective beta is outside acceptable range,
        # rebalance fund 1 / cash
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          cash.bal <- 0
          
        } else {
          
          if (! inside(effective.beta, beta.range)) {
            
            trades <- trades + 1
            if (inside(fund1.beta, maxall.beta.range)) {
              fund1.all <- 1
            } else {
              fund1.alls <- seq(0, 1, 0.001)
              port.betas <- fund1.alls * fund1.beta
              fund1.all <-
                fund1.alls[which.max(fund1.alls[inside(port.betas,
                                                       maxall.beta.range)])]
            }
            fund2.all <- 0
            cash.all <- 1 - fund1.all
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- 0
            cash.bal <- port.bal * cash.all
            
          }
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to fund 1 / cash.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            cash.bal <- 0
            
          } else {
            
            trades <- trades + 1
            if (inside(fund1.beta, maxall.beta.range)) {
              fund1.all <- 1
            } else {
              fund1.alls <- seq(0, 1, 0.001)
              port.betas <- fund1.alls * fund1.beta
              fund1.all <-
                fund1.alls[which.max(fund1.alls[inside(port.betas,
                                                       maxall.beta.range)])]
            }
            fund2.all <- 0
            cash.all <- 1 - fund1.all
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- 0
            cash.bal <- port.bal * cash.all
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.fund1.maxall <- list(fund.balances = fund.balances,
                                              fund.betas = fund.betas,
                                              effective.betas = effective.betas,
                                              trades = trades)
    
  }
  
  # Implement "fund2.maxall" failure method if requested
  if ("fund2.maxall" %in% failure.method) {
    
    # Calculate interval for starting beta when maximizing allocation to fund 1
    maxall.beta.range <- c(target.beta - maxall.tol, target.beta + maxall.tol)
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1 and fund 2
    if (! inside(fund1.all, c(0, 1))) {
      if (inside(fund2.beta, maxall.beta.range)) {
        fund2.all <- 1
      } else {
        fund2.alls <- seq(0, 1, 0.001)
        port.betas <- fund2.alls * fund2.beta
        fund2.all <-
          fund2.alls[which.max(fund2.alls[inside(port.betas,
                                                 maxall.beta.range)])]
      }
      fund1.all <- 0
      cash.all <- 1 - fund2.all
    } else {
      fund2.all <- 1 - fund1.all
      cash.all <- 0
    }
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    cash.bal <- initial * cash.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 4,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates, c(colnames(tickers.gains),
                                                     "Cash", "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal + cash.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, cash.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (cash.bal > 0 | fund2.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if effective beta is outside acceptable range,
        # rebalance fund 2 / cash
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          cash.bal <- 0
          
        } else {
          
          if (! inside(effective.beta, beta.range)) {
            
            trades <- trades + 1
            if (inside(fund2.beta, maxall.beta.range)) {
              fund2.all <- 1
            } else {
              fund2.alls <- seq(0, 1, 0.001)
              port.betas <- fund2.alls * fund2.beta
              fund2.all <-
                fund2.alls[which.max(fund2.alls[inside(port.betas,
                                                       maxall.beta.range)])]
            }
            fund1.all <- 0
            cash.all <- 1 - fund2.all
            fund1.bal <- 0
            fund2.bal <- port.bal * fund2.all
            cash.bal <- port.bal * cash.all
            
          }
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to fund 2 / cash.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            cash.bal <- 0
            
          } else {
            
            trades <- trades + 1
            if (inside(fund2.beta, maxall.beta.range)) {
              fund2.all <- 1
            } else {
              fund2.alls <- seq(0, 1, 0.001)
              port.betas <- fund2.alls * fund2.beta
              fund2.all <-
                fund2.alls[which.max(fund2.alls[inside(port.betas,
                                                       maxall.beta.range)])]
            }
            fund1.all <- 0
            cash.all <- 1 - fund2.all
            fund1.bal <- 0
            fund2.bal <- port.bal * fund2.all
            cash.bal <- port.bal * cash.all
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.fund2.maxall <- list(fund.balances = fund.balances,
                                              fund.betas = fund.betas,
                                              effective.betas = effective.betas,
                                              trades = trades)
    
  }
  
  # Implement "inverse1" failure method if requested
  if ("inverse1" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    if (inside(fund1.all, c(0, 1))) {
      fund2.all <- 1 - fund1.all
      inverse1.all <- 0
      cash.all <- 0
    } else {
      inverse1.all <- round((fund2.beta - target.beta) /
                              (fund1.beta + fund2.beta), 3)
      if (inside(inverse1.all, c(0, 1))) {
        fund1.all <- 0
        fund2.all <- 1 - inverse1.all
        cash.all <- 0
      } else {
        fund1.all <- 0
        fund2.all <- 0
        inverse1.all <- 0
        cash.all <- 1
      }
    }
    
    # Distribute initial balance to fund 1, fund 2, inverse fund 1, and cash
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    inverse1.bal <- initial * inverse1.all
    cash.bal <- initial * cash.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 5,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates,
                                            c(colnames(tickers.gains),
                                              paste("Inverse",
                                                    colnames(tickers.gains)[1]),
                                              "Cash", "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, inverse1.bal, cash.bal,
                            port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta -
      inverse1.all * fund1.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      inverse1.bal <- inverse1.bal * (1 - fund1.gains[ii])
      port.bal <- fund1.bal + fund2.bal + inverse1.bal + cash.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, inverse1.bal,
                                       cash.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal -
                           fund1.beta * inverse1.bal) / port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (cash.bal > 0) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if target beta can be achieved with inverse fund 1 /
        # fund 2, execute that trade.
        # (3) Otherwise, continue to hold 100% cash.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          inverse1.all <- 0
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          inverse1.bal <- 0
          cash.bal <- 0
          
        } else {
          
          inverse1.all <- round((fund2.beta - target.beta) /
                                  (fund1.beta + fund2.beta), 3)
          
          if (inside(inverse1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund1.all <- 0
            fund2.all <- 1 - inverse1.all
            cash.all <- 0
            fund1.bal <- 0
            fund2.bal <- port.bal * fund2.all
            inverse1.bal <- port.bal * inverse1.all
            cash.bal <- 0
            
          } else {
            
            fund1.all <- 0
            fund2.all <- 0
            inverse1.all <- 0
            cash.all <- 1
            fund1.bal <- 0
            fund2.bal <- 0
            inverse1.bal <- 0
            cash.bal <- port.bal
            
          }
          
        }
        
      } else if (inverse1.bal > 0) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if effective beta is outside acceptable range, execute
        # trade to rebalance inverse fund 1 / fund 2 if target beta is
        # achievable, otherwise switch to 100% cash.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          inverse1.all <- 0
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          inverse1.bal <- 0
          cash.bal <- 0
          
        } else {
          
          if (! inside(effective.beta, beta.range)) {
            
            inverse1.all <- round((fund2.beta - target.beta) /
                                    (fund1.beta + fund2.beta), 3)
            
            if (inside(inverse1.all, c(0, 1))) {
              
              trades <- trades + 1
              fund1.all <- 0
              fund2.all <- 1 - inverse1.all
              cash.all <- 0
              fund1.bal <- 0
              fund2.bal <- port.bal * fund2.all
              inverse1.bal <- port.bal * inverse1.all
              cash.bal <- 0
              
            } else {
              
              trades <- trades + 1
              fund1.all <- 0
              fund2.all <- 0
              inverse1.all <- 0
              cash.all <- 1
              fund1.bal <- 0
              fund2.bal <- 0
              inverse1.bal <- 0
              cash.bal <- port.bal
              
            }
            
          }
          
        }
        
      } else {
        
        # (1) If effective beta is outside acceptable range, execute trade to
        # rebalance fund 1 / fund 2.
        # (2) If target beta is not achievable, execute inverse fund 1 / fund 2
        # trade.
        # (3) If target beta is still not achievable, switch to 100% cash.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            inverse1.all <- 0
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            inverse1.bal <- 0
            cash.bal <- 0
            
          } else {
            
            inverse1.all <- round((fund2.beta - target.beta) /
                                    (fund1.beta + fund2.beta), 3)
            
            if (inside(inverse1.all, c(0, 1))) {
              
              trades <- trades + 1
              fund1.all <- 0
              fund2.all <- 1 - inverse1.all
              cash.all <- 0
              fund1.bal <- 0
              fund2.bal <- port.bal * fund2.all
              inverse1.bal <- port.bal * inverse1.all
              cash.bal <- 0
              
            } else {
              
              fund1.all <- 0
              fund2.all <- 0
              inverse1.all <- 0
              cash.all <- 1
              fund1.bal <- 0
              fund2.bal <- 0
              inverse1.bal <- 0
              cash.bal <- port.bal
              
            }
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.inverse1 <- list(fund.balances = fund.balances,
                                          fund.betas = fund.betas,
                                          effective.betas = effective.betas,
                                          trades = trades)
    
  }
  
  # Implement "inverse2" failure method if requested
  if ("inverse2" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    if (inside(fund1.all, c(0, 1))) {
      fund2.all <- 1 - fund1.all
      inverse2.all <- 0
      cash.all <- 0
    } else {
      inverse2.all <- round((fund1.beta - target.beta) /
                              (fund1.beta + fund2.beta), 3)
      if (inside(inverse2.all, c(0, 1))) {
        fund1.all <- 1 - inverse2.all
        fund2.all <- 0
        cash.all <- 0
      } else {
        fund1.all <- 0
        fund2.all <- 0
        inverse2.all <- 0
        cash.all <- 1
      }
    }
    
    # Distribute initial balance to fund 1, fund 2, inverse fund 2, and cash
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    inverse2.bal <- initial * inverse2.all
    cash.bal <- initial * cash.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- matrix(NA, ncol = 5,
                            nrow = nrow(tickers.gains) - window.units + 1,
                            dimnames = list(dates,
                                            c(colnames(tickers.gains),
                                              paste("Inverse",
                                                    colnames(tickers.gains)[2]),
                                              "Cash", "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, inverse2.bal, cash.bal,
                            port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta -
      inverse2.all * fund1.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      inverse2.bal <- inverse2.bal * (1 - fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal + inverse2.bal + cash.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, inverse2.bal,
                                       cash.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal -
                           fund2.beta * inverse2.bal) / port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (cash.bal > 0) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if target beta can be achieved with fund 1 / inverse
        # fund 2, execute that trade.
        # (3) Otherwise, continue to hold 100% cash.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          inverse2.all <- 0
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          inverse2.bal <- 0
          cash.bal <- 0
          
        } else {
          
          inverse2.all <- round((fund1.beta - target.beta) /
                                  (fund1.beta + fund2.beta), 3)
          
          if (inside(inverse2.all, c(0, 1))) {
            
            trades <- trades + 1
            fund1.all <- 1 - inverse2.all
            fund2.all <- 0
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- 0
            inverse2.bal <- port.bal * inverse2.all
            cash.bal <- 0
            
          } else {
            
            fund1.all <- 0
            fund2.all <- 0
            inverse2.all <- 0
            cash.all <- 1
            fund1.bal <- 0
            fund2.bal <- 0
            inverse2.bal <- 0
            cash.bal <- port.bal
            
          }
          
        }
        
      } else if (inverse2.bal > 0) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if effective beta is outside acceptable range, execute
        # trade to rebalance fund 1 / inverse fund 2 if target beta is
        # achievable, otherwise switch to 100% cash.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          inverse2.all <- 0
          cash.all <- 0
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          inverse2.bal <- 0
          cash.bal <- 0
          
        } else {
          
          if (! inside(effective.beta, beta.range)) {
            
            inverse2.all <- round((fund1.beta - target.beta) /
                                    (fund1.beta + fund2.beta), 3)
            
            if (inside(inverse2.all, c(0, 1))) {
              
              trades <- trades + 1
              fund1.all <- 1 - inverse2.all
              fund2.all <- 0
              cash.all <- 0
              fund1.bal <- port.bal * fund1.all
              fund2.bal <- 0
              inverse2.bal <- port.bal * inverse2.all
              cash.bal <- 0
              
            } else {
              
              trades <- trades + 1
              fund1.all <- 0
              fund2.all <- 0
              inverse2.all <- 0
              cash.all <- 1
              fund1.bal <- 0
              fund2.bal <- 0
              inverse2.bal <- 0
              cash.bal <- port.bal
              
            }
            
          }
          
        }
        
      } else {
        
        # (1) If effective beta is outside acceptable range, execute trade to
        # rebalance fund 1 / fund 2.
        # (2) If target beta is not achievable, execute fund 1 / inverse fund 2
        # trade.
        # (3) If target beta is still not achievable, switch to 100% cash.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            inverse2.all <- 0
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            inverse2.bal <- 0
            cash.bal <- 0
            
          } else {
            
            inverse2.all <- round((fund1.beta - target.beta) /
                                    (fund1.beta + fund2.beta), 3)
            
            if (inside(inverse2.all, c(0, 1))) {
              
              trades <- trades + 1
              fund1.all <- 1 - inverse2.all
              fund2.all <- 0
              cash.all <- 0
              fund1.bal <- port.bal * fund1.all
              fund2.bal <- 0
              inverse2.bal <- port.bal * inverse2.all
              cash.bal <- 0
              
            } else {
              
              fund1.all <- 0
              fund2.all <- 0
              inverse2.all <- 0
              cash.all <- 1
              fund1.bal <- 0
              fund2.bal <- 0
              inverse2.bal <- 0
              cash.bal <- port.bal
              
            }
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.inverse2 <- list(fund.balances = fund.balances,
                                          fund.betas = fund.betas,
                                          effective.betas = effective.betas,
                                          trades = trades)
    
  }
  
  # Implement "closer" failure method if requested
  if ("closer" %in% failure.method) {
    
    # Calculate initial betas and initial target allocation to fund 1
    fund1.beta <- fund.betas[1, 1]
    fund2.beta <- fund.betas[1, 2]
    fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                       3)
    
    # Distribute initial balance to fund 1 and fund 2
    if (inside(fund1.all, c(0, 1))) {
      fund2.all <- 1 - fund1.all
    } else {
      fund1.all <- ifelse(which.min(abs(c(fund1.beta, fund2.beta) -
                                          target.beta)) == 1, 1, 0)
      fund2.all <- 1 - fund1.all
    }
    fund1.bal <- initial * fund1.all
    fund2.bal <- initial * fund2.all
    port.bal <- initial
    
    # Initialize matrix for fund balances
    fund.balances <- 
      matrix(NA, ncol = 3, nrow = nrow(tickers.gains) - window.units + 1, 
             dimnames = list(dates, c(colnames(tickers.gains), "Portfolio")))
    fund.balances[1, ] <- c(fund1.bal, fund2.bal, port.bal)
    
    # Initialize vector for effective betas
    effective.beta <- fund1.all * fund1.beta + fund2.all * fund2.beta
    effective.betas <- rep(NA, nrow(tickers.gains) - window.units + 1)
    names(effective.betas) <- dates
    effective.betas[1] <- effective.beta
    
    # Loop through and implement target-beta strategy
    trades <- 0
    loop.index <- 1
    for (ii in (window.units + 1): nrow(tickers.gains)) {
      
      # Within-loop index
      loop.index <- loop.index + 1
      
      # Apply gains on iith day
      fund1.bal <- fund1.bal * (1 + fund1.gains[ii])
      fund2.bal <- fund2.bal * (1 + fund2.gains[ii])
      port.bal <- fund1.bal + fund2.bal
      fund.balances[loop.index, ] <- c(fund1.bal, fund2.bal, port.bal)
      
      # Get fund 1 and fund 2 betas for time period of interest
      fund1.beta <- fund.betas[loop.index, 1]
      fund2.beta <- fund.betas[loop.index, 2]
      
      # Calculate target allocation for fund 1
      fund1.all <- round((target.beta - fund2.beta) / (fund1.beta - fund2.beta),
                         3)
      
      # Calculate effective beta
      effective.beta <- (fund1.beta * fund1.bal + fund2.beta * fund2.bal) /
        port.bal
      effective.betas[loop.index] <- effective.beta
      
      # Rebalance
      if (fund1.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if fund 1 still has beta closer to target, stick with
        # 100% fund 1.
        # (3) Otherwise, switch to 100% fund 2.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          
        } else {
          
          if (which.min(abs(c(fund1.beta, fund2.beta) - target.beta)) == 1) {
            
            fund1.all <- 1
            fund2.all <- 0
            fund1.bal <- port.bal
            fund2.bal <- 0
            
          } else {
            
            trades <- trades + 1
            fund1.all <- 0
            fund2.all <- 1
            fund1.bal <- 0
            fund2.bal <- port.bal
            
          }
          
        }
        
      } else if (fund2.bal == port.bal) {
        
        # (1) If target beta can be achieved with fund 1 / fund 2, execute that
        # trade.
        # (2) Otherwise, if fund 2 still has beta closer to target, stick with
        # 100% fund 2.
        # (3) Otherwise, switch to 100% fund 1.
        
        if (inside(fund1.all, c(0, 1))) {
          
          trades <- trades + 1
          fund2.all <- 1 - fund1.all
          fund1.bal <- port.bal * fund1.all
          fund2.bal <- port.bal * fund2.all
          
        } else {
          
          if (which.min(abs(c(fund1.beta, fund2.beta) - target.beta)) == 2) {
            
            fund1.all <- 0
            fund2.all <- 1
            fund1.bal <- 0
            fund2.bal <- port.bal
            
          } else {
            
            trades <- trades + 1
            fund1.all <- 1
            fund2.all <- 0
            fund1.bal <- port.bal
            fund2.bal <- 0
            
          }
          
        }
        
      } else {
        
        # If effective beta is outside acceptable range, execute rebalancing
        # trade if target beta is achievable, otherwise switch to 100% whichever
        # fund has beta closer to target.
        
        if (! inside(effective.beta, beta.range)) {
          
          if (inside(fund1.all, c(0, 1))) {
            
            trades <- trades + 1
            fund2.all <- 1 - fund1.all
            cash.all <- 0
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            
          } else {
            
            trades <- trades + 1
            fund1.all <- ifelse(which.min(abs(c(fund1.beta, fund2.beta) -
                                                target.beta)) == 1, 1, 0)
            fund2.all <- 1 - fund1.all
            fund1.bal <- port.bal * fund1.all
            fund2.bal <- port.bal * fund2.all
            
          }
          
        }
        
      }
      
    }
    
    # If reference funds provided, add to fund.balances matrix
    if (! is.null(reference.gains)) {
      
      fund.balances <- 
        cbind(fund.balances, apply(reference.gains, 2, function(x) 
          gains_prices(gains = x[(window.units + 1): length(x)], 
                       initial = initial)))
      
    }
    
    # Combine results into list, and add it to growing list to return to user
    results.list$failure.closer <- list(fund.balances = fund.balances,
                                        fund.betas = fund.betas,
                                        effective.betas = effective.betas,
                                        trades = trades)
    
  }
  
  # Return list of results
  if (length(results.list) == 1) {
    results.list <- results.list[[1]]
  }
  return(results.list)
  
}
