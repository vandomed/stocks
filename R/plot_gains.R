#' Plot Gains for One Investment vs. Another
#' 
#' Useful for visualizing how two investments behave relate to each other, or 
#' how several investments behave relative to the same benchmark. 
#' 
#' 
#' @param formula Formula, e.g. \code{SSO + UPRO ~ SPY} to plot gains for SSO 
#' and UPRO vs. SPY.
#' @param ... Arguments to pass along with \code{tickers} to 
#' \code{\link{load_gains}}.
#' @param gains Data frame with one column of gains for each investment 
#' mentioned in \code{formula}. If unspecified, function downloads historical 
#' gains internally.
#' @param prices Data frame with one column of prices for each investment 
#' mentionedin \code{formula}.
#' @param poly_order Numeric value specifying the polynomial order for linear 
#' regression, e.g. \code{1} for simple linear regression or \code{2} for 
#' linear regression with first- and second-order terms.
#' @param return Character string specifying what to return. Choices are 
#' \code{"plot"}, \code{"data"}, and \code{"both"}.
#' 
#' 
#' @return
#' In addition to the graph, a list containing fitted linear regression models 
#' returned by \code{\link[stats]{lm}} for each investment vs. the benchmark.
#' 
#' 
#' @references 
#' Jeffrey A. Ryan and Joshua M. Ulrich (2019). quantmod: Quantitative Financial 
#' Modelling Framework. R package version 0.4-15. 
#' \url{https://CRAN.R-project.org/package=quantmod}
#' 
#' 
#' @examples
#' \dontrun{
#' # Plot daily gains for SSO and UPRO vs. VFINX
#' p <- plot_gains(SSO + UPRO ~ VFINX)
#' }
#'
#' @export
plot_gains <- function(formula = BRK.B ~ VFINX, ..., 
                       gains = NULL,
                       prices = NULL,
                       poly_order = 1, 
                       return = "plot") {
  
  # Extract info from formula
  tickers <- all.vars(formula)
  x.ticker <- tickers[length(tickers)]
  y.tickers <- tickers[-length(tickers)]
  
  # Obtain gains data frame
  if (! is.null(prices)) {
    prices <- prices[complete.cases(prices), , drop = FALSE]
    gains <- prices_gains(prices = prices)
  }
  if (is.null(gains)) {
    gains <- load_gains(tickers = tickers, mutual.start = TRUE, mutual.end = TRUE, ...)
  }
  
  # Transform for ggplot
  gains <- gains %>%
    mutate_at(
      .vars = tickers, 
      .funs = function(x) x * 100
    )
  df <- gains[tickers] %>% 
    as.data.table() %>% 
    melt(measure.vars = y.tickers, 
         variable.name = "Fund", 
         value.name = "Gain")

  # Create plot
  p <- ggplot(df, aes(x = .data[[x.ticker]], y = Gain, group = Fund)) + 
    facet_wrap(~Fund) + 
    geom_hline(yintercept = 0, lty = 2) + 
    geom_vline(xintercept = 0, lty = 2) + 
    geom_point() + 
    labs(title = paste("Scatterplot of Gains vs.", x.ticker), 
         y = "Gains (%)", 
         x = paste("Gains for", x.ticker, "(%)")) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # Add regression line/curve if requested
  if (! is.null(poly_order)) {
    
    if (poly_order == 1) {
      
      fits <- lapply(y.tickers, function(x) {
        lm(paste(x, "~", x.ticker))
      }, data = gains)
      
      b0 <- sprintf("%.3f", sapply(fits, function(x) x$coef[1]))
      b1 <- sprintf("%.2f", sapply(fits, function(x) x$coef[2]))
      r2 <- sprintf("%.2f", sapply(fits, function(x) summary(x)$r.squared))
      labels <- mapply(function(x, y, z) {
        bquote(alpha == ~ .(x)*", "*beta == .(y)*", "*{R^2 == .(z)})
      }, x = b0, y = b1, z = r2, USE.NAMES = FALSE)
      
      p <- p + 
        geom_smooth(aes(color = Fund), formula = y ~ x, method = "lm", 
                    se = FALSE, show.legend = TRUE) + 
        scale_colour_manual(values = hue_pal()(length(y.tickers)), 
                            labels = labels, name = "Regression line")
      p
      
    } else {
      
      fits <- lapply(y.tickers, function(x) 
        lm(paste(x, " ~ poly(", x.ticker, ", ", 
                 poly_order, ", raw = TRUE)", sep = ""), data = gains))
      
      b0 <- sprintf("%.3f", sapply(fits, function(x) x$coef[1]))
      r2 <- sprintf("%.2f", sapply(fits, function(x) summary(x)$r.squared))
      labels <- mapply(function(x, z) {
        bquote(alpha == ~ .(x)*", "*{R^2 == .(z)})
      }, x = b0, z = r2, USE.NAMES = FALSE)
      
      p <- p + 
        stat_smooth(aes(color = Fund), 
                    formula = y ~ poly(x, poly_order, raw = TRUE), 
                    method = "lm", se = FALSE, show.legend = TRUE) + 
        scale_colour_manual(values = hue_pal()(length(y.tickers)), 
                            labels = labels, name = "Regression curve")
      
    }
    
  }
  
  if (return == "plot") return(p)
  if (return == "data") return(df)
  if (return == "both") return(list(plot = p, data = df))
  return(list(plot = p, regressions = fits))
  
  
}
