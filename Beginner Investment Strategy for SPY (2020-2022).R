# installing Rtools42 with default settings from (https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html)
# from Rtools install pkgbuild and then quantstrat
install.packages("pkgbuild")
remotes::install_github("braverock/quantstrat", force = TRUE)

# loading packages: xts, zoo, quantmod, FinancialInstrument, TTR, and quantstrat


# specifying initialization date (for backtest), and lower and upper date limits
initdate <- "2019-12-15"
from <- "2020-06-01"
to <- "2022-12-14"

# setting timezone and currency
Sys.setenv(TZ = "UTC")
currency("USD")

# importing historic financial data from yahoo and initializing LQD for strategy
getSymbols("SPY", src = "yahoo", from = from, to = to, adjust = TRUE)
stock("SPY", currency = "USD")

# defining tradesize and initial equity
tradesize <- 100000
initeq <- 100000

# objects in financial trading: account, portfolio, strategy
# naming first strategy, portfolio, and account
strategy.st <- "beginnerstrategy"
portfolio.st <- "beginnerstrategy"
account.st <- "beginnerstrategy"

# initialize portfolio, then account, orders, and stategy
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)

# create trend and reversion indicators with customized values
add.indicator(strategy=strategy.st, name="SMA", arguments = list(x=quote(Cl(mktdata)), n=130), label="SMA2q")
add.indicator(strategy=strategy.st, name="SMA", arguments = list(x=quote(Cl(mktdata)), n=35), label="SMA7w")
DVO <- function(HLC, navg = 2, percentlookback = 126) {
	ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
	avgratio <- SMA(ratio, n = navg)
	percentile <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
	colnames(percentile) <- "DVO"
	return(percentile)
}
add.indicator(strategy=strategy.st, name="DVO", 
arguments=list(HLC=quote(HLC(mktdata)), 
navg = 2, percentlookback = 126), label = "DVO_2_126")

# test our indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
test["2022-08-01/2022-09-01"]


# add signals for scenarios when the short-term average is above the long-term average, when it crosses under,
# when the oscillator remains low, and when it crosses a high threshold
add.signal(strategy.st, name = "sigComparison",
arguments = list(columns = c("SMA7w", "SMA2q"),
relationship = "gt"), label = "short_sma_over")

add.signal(strategy.st, name = "sigCrossover",
arguments = list(columns = c("SMA7w", "SMA2q"),
relationship = "lt"), label = "short_sma_crosses_under")

add.signal(strategy.st, name = "sigThreshold",
arguments = list(column = "DVO_2_126",
threshold = 20, relationship = "lt", cross = FALSE),
label = "low_oscillator")

add.signal(strategy.st, name = "sigThreshold",
arguments = list(column = "DVO_2_126",
threshold = 80, relationship = "gt", cross = TRUE),
label = "thresholdexit")

# test if signals were added correctly
test_init <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)
test["2022-08-01/2022-09-01"]

# combine signals that indicate to enter a position when both the short_sma_over and low_oscillator become true
add.signal(strategy.st, name = "sigFormula",
arguments = list(formula = "short_sma_over & low_oscillator",
cross = TRUE), label = "longentry")


# add rules to shape transaction behaviors based on signals
# rule for exiting position when short-term SMA becomes lower than long-term SMA, indicating an upcoming downward trend 
add.rule(strategy.st, name = "ruleSignal", 
arguments = list(sigcol = "short_sma_crosses_under", sigval = TRUE, 
orderqty = "all", ordertype = "market", orderside = "long",
replace = FALSE, prefer = "Open"), type = "exit")

# rule for exiting position when oscillator becomes too high - too risky
add.rule(strategy.st, name = "ruleSignal", 
arguments = list(sigcol = "thresholdexit", sigval = TRUE, 
orderqty = "all", ordertype = "market", orderside = "long",
replace = FALSE, prefer = "Open"), type = "exit")

# to add final rule, install and load IKTrading package from github (author: Ilya Kipnis)
remotes::install_github("IlyaKipnis/IKTrading")

# rule on combined signal to enter position where short_sma_over and low_oscillator become true
add.rule(strategy.st, name = "ruleSignal", 
arguments=list(sigcol = "longentry", sigval = TRUE,
ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open", 
osFUN = osMaxDollar, tradeSize = tradesize, maxSize = tradesize),
type = "enter")

# applying strategy and updating portfolio and account
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)

# retrieving strategy statistics
strategy_stats <- tradeStats(Portfolios = portfolio.st)
strategy_stats$Profit.Factor

# as you can see from the chart, the strategy stops being effective in the beginning of 2022
# this can be explained through a wide range of geo-political tensions and crises that had an effect on market volatility
# the strategy can be improved through organizing to take advantage of the volatility in year 2022, but some parameters need to be adjusted (such as the simple moving averages)
sma7w <- SMA(x = Cl(SPY), n = 35)
sma2q <- SMA(x = Cl(SPY), n = 130)
DVO_2_126 <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")
add_TA(sma7w, on = 1, col = "blue")
add_TA(sma2q, on = 1, col = "red")
add_TA(DVO_2_126)

# computing standard return-based Sharpe ratio
instrument_returns <- PortfReturns(portfolio.st)
SharpeRatio.annualized(instrument_returns, geometric = FALSE)

