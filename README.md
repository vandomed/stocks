Get Rich with 'stocks'
================
Dane Van Domelen <br> <vandomed@gmail.com>
2018-08-22

<!-- README.md is generated from README.Rmd. Please edit that file -->
'stocks' package
----------------

The **stocks** package has a variety of functions for analyzing investments and investment strategies. I use it for a lot of my articles on [Seeking Alpha](https://seekingalpha.com/author/dane-van-domelen/articles#regular_articles). The package relies heavily on [Yahoo! Finance](https://finance.yahoo.com/) for historical prices and on the **quantmod** package for downloading that data into R.

There are functions for calculating performance metrics, backtesting trading strategies, and visualizing the performance of funds or strategies. I would say it is similar in spirit to the website [Portfolio Visualizer](https://www.portfoliovisualizer.com/) and the R package **PerformanceAnalytics**.

Like most vignettes, the purpose here will be to illustrate typical usage of the functions in **stocks**. I'll try to incorporate some interesting examples.

To start, here are the functions:

<table>
<colgroup>
<col width="33%" />
<col width="66%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="left">Purpose</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><em>load_prices</em></td>
<td align="left">Download and align historical prices for a set of tickers.</td>
</tr>
<tr class="even">
<td align="left"><em>load_gains</em></td>
<td align="left">Download and align gains for a set of tickers.</td>
</tr>
<tr class="odd">
<td align="left"><em>prices_rate</em></td>
<td align="left">Calculate growth rate from a vector of prices.</td>
</tr>
<tr class="even">
<td align="left"><em>gains_rate</em></td>
<td align="left">Calculate growth rate from a vector of gains.</td>
</tr>
<tr class="odd">
<td align="left"><em>sharpe</em></td>
<td align="left">Sharpe ratio.</td>
</tr>
<tr class="even">
<td align="left"><em>sortino</em></td>
<td align="left">Sortino ratio.</td>
</tr>
<tr class="odd">
<td align="left"><em>mdd</em></td>
<td align="left">Maximum drawdown.</td>
</tr>
<tr class="even">
<td align="left"><em>rrr</em></td>
<td align="left">Risk-return ratio.</td>
</tr>
<tr class="odd">
<td align="left"><em>metrics</em></td>
<td align="left">Calculate various performance metrics.</td>
</tr>
<tr class="even">
<td align="left"><em>twofunds_graph</em></td>
<td align="left">Graph one performance metric vs. another for two-fund portfolios as allocation varies.</td>
</tr>
<tr class="odd">
<td align="left"><em>threefunds_graph</em></td>
<td align="left">Graph One performance metric vs. another for three-fund portfolios as allocation varies.</td>
</tr>
<tr class="even">
<td align="left"><em>onemetric_graph</em></td>
<td align="left">Graph performance metric for various investments.</td>
</tr>
<tr class="odd">
<td align="left"><em>onemetric_overtime_graph</em></td>
<td align="left">Graph performance metric over time for various investments.</td>
</tr>
<tr class="even">
<td align="left"><em>twometrics_graph</em></td>
<td align="left">Graph one performance metric vs. another for various investments.</td>
</tr>
<tr class="odd">
<td align="left"><em>targetbeta_twofunds</em></td>
<td align="left">Backtest a two-fund strategy that targets a certain beta.</td>
</tr>
<tr class="even">
<td align="left"><em>targetall</em></td>
<td align="left">Backtest a fixed-allocation trading strategy.</td>
</tr>
</tbody>
</table>

Loading data from Yahoo! Finance
--------------------------------

The functions *load\_prices* and *load\_gains* let you download historical data from Yahoo! Finance. They use **quantmod**'s *getSymbols* function internally. Here are some examples. (Note: R output is printed in nice tables rather than usual-looking R output because I loaded **printr**)

``` r
library("stocks")
library("printr")
prices <- load_prices(c("SPY", "TLT"), to = "2018-03-02")
head(prices)
```

|            |    SPY|    TLT|
|------------|------:|------:|
| 2002-07-30 |  66.29|  45.04|
| 2002-07-31 |  66.45|  45.59|
| 2002-08-01 |  64.72|  45.85|
| 2002-08-02 |  63.26|  46.32|
| 2002-08-05 |  61.06|  46.53|
| 2002-08-06 |  63.12|  46.13|

``` r

gains <- load_gains(c("SPY", "TLT"), to = "2018-03-02")
head(gains)
```

|            |      SPY|      TLT|
|------------|--------:|--------:|
| 2002-07-31 |   0.0024|   0.0124|
| 2002-08-01 |  -0.0261|   0.0057|
| 2002-08-02 |  -0.0224|   0.0102|
| 2002-08-05 |  -0.0348|   0.0044|
| 2002-08-06 |   0.0337|  -0.0085|
| 2002-08-07 |   0.0174|   0.0024|

In either case, data going as far back as possible for SPY and TLT (which happens to be mid-2002) are retrieved.

Performance metrics
-------------------

### Total or annualized growth, Sharpe ratio, max drawdown

There are functions for calculating various performance metrics based on a vector of prices or gains, or a matrix where each column has prices or gains for a different fund. To illustrate a few, here's net growth based on either data type:

``` r
prices_rate(prices)
#>   SPY   TLT 
#> 3.025 1.599
gains_rate(gains)
#>   SPY   TLT 
#> 3.025 1.599
```

Values of 3.025 and 1.599 mean SPY grew 302.5% and TLT 159.9%. For annualized rather than total growth:

``` r
prices_rate(prices, units.rate = 252)
#>     SPY     TLT 
#> 0.09352 0.06324
gains_rate(gains, units.rate = 252)
#>     SPY     TLT 
#> 0.09352 0.06324
```

Sharpe ratio and max drawdown:

``` r
sharpe(prices = prices)
#>     SPY     TLT 
#> 0.03608 0.03271
sharpe(gains = gains)
#>     SPY     TLT 
#> 0.03608 0.03271

mdd(prices = prices)
#>    SPY    TLT 
#> 0.5519 0.2659
mdd(gains = gains)
#>    SPY    TLT 
#> 0.5519 0.2659
```

### All-in-one

Better yet, a single call to *metrics* produces a data frame with all of these metrics (and potentially others) and a correlation matrix for gains.

``` r
bond.metrics <- metrics(gains = gains, perf.metrics = c("growth", "cagr", "sharpe", "mdd"))
bond.metrics$perf.metrics
```

|     |  Growth|    CAGR|  Sharpe|     MDD|
|-----|-------:|-------:|-------:|-------:|
| SPY |   3.025|  0.0935|  0.0361|  0.5519|
| TLT |   1.599|  0.0632|  0.0327|  0.2659|

``` r
bond.metrics$cor.mat
```

|     |      SPY|      TLT|
|-----|--------:|--------:|
| SPY |   1.0000|  -0.4066|
| TLT |  -0.4066|   1.0000|

Looking at the performance metrics, we see that SPY achieved almost 2x the growth of TLT, but also had a max drawdown more than 2x as severe. A higher Sharpe ratio suggests better risk-adjusted returns for SPY.

Graphics
--------

### Growth of $10k

Let's look at the famous FANG stocks (Facebook, Amazon, Netflix, Google) for the past 3 years, with SPY (an S&P 500 ETF) included for reference.

``` r
fig <- growth_graph(c("SPY", "FB", "AMZN", "NFLX", "GOOG"), from = "2015-03-02", 
                    to = "2018-03-02")
```

![](README-unnamed-chunk-7-1.png)

Notice that I specified a vector of ticker symbols, not a matrix of prices or gains. All of the graphing functions in *stocks* have the ability to download data on the fly.

Clearly the FANG stocks have continued with their remarkable growth over the past 3 years. I actually held Amazon and Google briefly, but sold them once I decided I couldn't beat the market through stock-picking. I can't, but in this case I wish I would have kept trying.

### Gains vs. benchmark (alpha, beta, R-squared)

Before buying a fund, you should have a rough idea of how it tends to move with the market. The classic way of doing this is plotting the fund's daily gains vs. a benchmark (typically the S&P) and looking at the estimated intercept ("alpha") and slope ("beta"). For example, for Vanguard's long-term bond fund VBLTX, over the past 5 years:

``` r
fig <- gains_graph(c("SPY", "VBLTX"), from = "2013-03-02", to = "2018-03-02")
```

![](README-unnamed-chunk-8-1.png)

Definitely a negative trend there, although a weak one: *R*<sup>2</sup> = 0.08 means only 8% of variability in VBLTX gains are explained by the regression on SPY.

Looking at the regression equation, we see positive alpha and negative beta, suggesting VBLTX and SPY would complement each other nicely in a retirement portfolio.

### Compare funds on 1 metric

To compare annualized growth for the 10 Select Sector SPDR ETFs over the past year:

``` r
data(sector_spdr_etfs)
fig <- onemetric_graph(tickers = sector_spdr_etfs$ticker, 
                       from = "2017-03-02", to = "2018-03-02")
```

![](README-unnamed-chunk-9-1.png)

Notice that I loaded the 10 ticker symbols with `data(sector_spdr_etfs)`. That worked because a dataset called `sector_spdr_etfs` is included in **stocks**. Quite a few other ticker lists are also available (run `data(package = "stocks")` to see them).

Looks like only 6 of the 10 sectors had positive growth over the past year; XLK (technology) was the best performer with 30.3% growth.

### Compare funds on 2 metrics

To simultaneousy look at annualized growth and maximum drawdown for the sector ETFs:

``` r
fig <- twometrics_graph(tickers = sector_spdr_etfs$ticker, 
                        from = "2017-03-02", to = "2018-03-02", 
                        x.metric = "mdd", y.metric = "cagr")
```

![](README-unnamed-chunk-10-1.png)

Here XLY (consumer discretionary) looks pretty good, with the best max drawdown and second-best growth.

### One metric over time

Let's move to another example: Warren Buffett's Berkshire Hathaway (BRK-B). Let's see whether BRK-B has consistently outperformed SPY over the past 15 years, looking at the 100-day moving Sharpe ratio:

``` r
fig <- onemetric_overtime_graph(tickers = c("SPY", "BRK-B"), 
                                from = "2003-03-02", to = "2018-03-02", 
                                window.units = 100, y.metric = "sharpe")
```

![](README-unnamed-chunk-11-1.png)

There are certainly some periods where SPy does a lot better, and some where BRK-B does a lot better. There's no obvious winner here, visually. The actual 100-day Sharpe ratios are returned by *onemetric\_overtime\_graph*, so we can compare the medians and see what percent of the time BRK-B's is higher:

``` r
apply(fig, 2, median)
#>     SPY   BRK-B 
#> 0.07349 0.05481
mean(fig[, 2] > fig[, 1])
#> [1] 0.4331
```

BRK-B has a tendency for worse risk-adjusted returns than SPY - not good, Mr. Buffett!

### 2-fund portfolio optimization

Until recently, my own retirement portfolio was simply 1/3 UPRO, 2/3 VBLTX. UPRO is a 3x daily S&P 500 ETF, and VBLTX is Vanguard's long-term bond mutual fund. A good way of visualizing the characteristics of a two-fund portfolio is to plot mean vs. standard deviation of daily gains, for allocations ranging from 100% fund 1 to 100% fund 2. To do that, using data over UPRO's lifetime, going back to 6/25/09:

``` r
fig <- twofunds_graph(tickers = c("UPRO", "VBLTX"), reference.tickers = "SPY",  
                      to = "2018-03-02")
```

![](README-unnamed-chunk-13-1.png)

The UPRO/VBLTX strategy sort of trumps SPY, because there are regions on the curve with higher returns and lower volatility than SPY.

For another example, we, could look at annualized growth vs. maximum drawdown rather than mean vs. standard deviation, and look at UPRO/VBLTX as well as UPRO/VBMFX, to see how swapping the long-term fund VBLTX with the total bond fund VBMFX would affect our portfolio.

``` r
fig <- twofunds_graph(tickers = c("UPRO", "VBLTX", "UPRO", "VBMFX"), 
                      reference.tickers = "SPY",  
                      to = "2018-03-02", 
                      x.metric = "mdd", y.metric = "cagr")
```

![](README-unnamed-chunk-14-1.png)

UPRO/VBLTX clearly has much better performance. One more example, this time Sharpe ratio vs. allocation, for the same two funds plus UPRO/TLT:

``` r
fig <- twofunds_graph(tickers = c("UPRO", "VBLTX", "UPRO", "VBMFX", "UPRO", "TLT"), 
                      reference.tickers = "SPY",  
                      to = "2018-03-02", 
                      x.metric = "mean", y.metric = "sharpe", 
                      plot.list = list(ylim = c(0, 0.12)))
```

![](README-unnamed-chunk-15-1.png)

The rationale for this graph is that we might want to target the same level of expected returns as SPY, but with as high of a Sharpe ratio as possible. If you go straight up on the graph from the SPY data point, you hit the red curve first, then the orange, and finally the blue. So we would choose UPRO/VBLTX, with something like 75-80% allocated to VBLTX.

### 3-fund portfolio optimization

The change I made recently was adding Vanguard's high-yield (aka "junk bond") mutual fund, VWEHX, to the mix. To visualize characteristics of VWEHX/UPRO/VBLTX:

``` r
fig <- threefunds_graph(tickers = c("VWEHX", "UPRO", "VBLTX"), 
                        reference.tickers = "SPY",  
                        to = "2018-03-02")
```

![](README-unnamed-chunk-16-1.png)

Each curve is for a fixed allocation to the first fund. The black curve is 0% VWEHX, so it is the same as the UPRO/VBLTX curve in the previous graph. The red curve is 20% VWEHX, and the remaining 80% allocated entirely to VBLTX (bottom-most point), entirely to UPRO (top-right data point), and everywhere in between.

At SPY's SD, the black curve (0% VWEHX) achieves the highest mean. At SPY's mean, the blue (40% VWEHX) or red (20% VWEHX) curve has the lowest SD. So depending on what you're going for, it may or may not be worthwhile adding VWEHX. My rationale was sort of qualitative: two nearly independent alpha-generating bond funds seems better than one.

Conclusions
-----------

Hopefully this gives you a good idea of the main functions in **stocks**. There are a lot of options I didn't cover, especially for the graphics functions. You can read about these options by looking at the relevant help files (e.g. run `?twofunds_graph` in R). Please feel free to e-mail me at <vandomed@gmail.com> with questions or suggestions, or, better yet, contribute to the project on [GitHub](https://github.com/vandomed/stocks).

References
----------

Peterson, Brian G., and Peter Carl. 2014. *PerformanceAnalytics: Econometric Tools for Performance and Risk Analysis*. <https://CRAN.R-project.org/package=PerformanceAnalytics>.

Ryan, Jeffrey A., and Joshua M. Ulrich. 2017. *Quantmod: Quantitative Financial Modelling Framework*. <https://CRAN.R-project.org/package=quantmod>.

Xie, Yihui. 2017. *Printr: Automatically Print R Objects to Appropriate Formats According to the ’Knitr’ Output Format*. <https://CRAN.R-project.org/package=printr>.
