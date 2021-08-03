

###Stock Trading using Technical Analysis Indicators: APP and RSI

### Lets pull some data:
# Create a Data Set by pulling trading symbol for Apple
getSymbols("AAPL")
# Display the data 
head(AAPL)
# Lets extract info from each column. use the first 2 letters of the Index to # pull data:
# Open 
Op(AAPL)   
# Close
Cl(AAPL)
# High
Hi(AAPL)
# Low 
Lo(AAPL)
# Volume
Vo(AAPL)
# AdjClose
Ad(AAPL)

### AN EXAMPLE OF SHORTING A STOCK### 
# Let’s say you buy a stock with the expectation that the stock will increase 
# in value, with a plan to sell the stock at a higher price. 
# This is a long position: you are holding a financial asset for which you 
# will profit if the asset increases in value. Your potential profit is unlimited,
# and your potential losses are limited by the price of the stock since 
# stock prices never go below zero. 

# On the other hand, if you expect a stock to decrease in value, 
# you may borrow the stock from a brokerage firm and sell it, 
# with the expectation of buying the stock back later at a lower price, 
# thus earning you a profit. This is called shorting a stock, and is a 
# short position, since you will earn a profit if the stock drops in value.

# Ok. Lets Import the specific dates for AAPL. 

# Most recent years data or LAST years to evaluate!
last(AAPL,'1 year')
# Display Results 
"AAPL"
# Lets look at volume and check it out by week.# sum from Monday to Friday
apply.weekly(Vo(AAPL),sum)
# By Month 
apply.monthly(Vo(AAPL),sum)
# By Quarter 
apply.quarterly(Vo(AAPL),sum)
# By year-to-date
apply.yearly(Vo(AAPL),sum)
# Lets look at the mean so we can analyze stocks behavior
apply.weekly(Vo(AAPL),mean)

# Any trader must have a set of rules that determine how much of her money 
# she is willing to bet on any single trade.This is called an EXIT STRATEGY.
# in any trade, a trader must have an exit strategy, 
# a set of conditions determining when she will exit the position, 
# for either profit or loss. A trader may set a target, which is the minimum 
# profit that will induce the trader to leave the position. Likewise, a trader 
# must have a maximum loss she is willing to tolerate; if potential losses go 
# beyond this amount, the trader will exit the position in order to prevent any
# further loss (this is usually done by setting a stop-loss order, 
# an order that is triggered to prevent further losses).

## Any plan that includes trading signals for initiating exit strategies, 
# are the traders rules for deciding how much of the portfolio to risk on any 
# particular strategy.
# Our concern now is to design and evaluate some trading strategies.


# Lets make a line graph y-t-d
chartSeries(AAPL,
            type="line",
            subset='2021',
            theme=chartTheme('white'))

# Specific month of Candlestick Action MAY 2021
chartSeries(AAPL,
            type="candlesticks",
            subset='2021-05',
            theme=chartTheme('white'))
            
# Ok lets check out the candles for resistance between 5/15 - 5/30
chartSeries(AAPL,
            type="auto",
            subset='2021-05-10::2021-05-30',
            theme=chartTheme('white'))


# OK. Now we want to load a new library for Technical analysis. 
install.packages("TTR")

# Check the library imported 
library(TTR)

# Lets try to trouble shoot: Close will not load 
tickers <- c("AAPL")
# VERIFY DATES OF DATASET 1/2021 - 7/2021
getSymbols(tickers, from="2021-01-04", to="2021-07-27")
ClosePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
head(ClosePrices)

# SMA n=days across moving average 5 DAY CALCULATION
SMA(Cl(AAPL),n=05)

# EMA 5 DAY CALCULATION
EMA(Cl(AAPL),n=05)

# Bollinger Bands, a technical indicator developed by John Bollinger, 
# are used to measure a market's volatility and identify “overbought” 
# or “oversold” conditions. Basically, this little tool tells us whether 
# the market is quiet or whether the market is LOUD
# Bollinger Band Code

BBands(Cl(AAPL),s.d=2)

#### Here, I will be demonstrating a moving average crossover strategy. 
# We will use two moving averages, one we consider “fast”, and the other “slow”. 
# The strategy is:
# Trade the asset when the fast moving average crosses over the slow 
# moving average. 
# Exit the trade when the fast moving average crosses over the slow 
# moving average again. A long trade will be prompted when the fast 
# moving average crosses from below to above the slow moving average, 
# and the trade will be exited when the fast moving average crosses below 
# the slow moving average later. A short trade will be prompted when the 
# fast moving average crosses below the slow moving average, and the trade will be exited when the fast moving average later crosses above the slow moving average.

# Lets code for Momentum: is it fast or slow across 5 days?
momentum(Cl(AAPL), n=5)

# We now have a complete strategy. 
# But before we decide we want to use it, we should try to evaluate 
# the quality of the strategy first. The usual means for doing so is 
# backtesting, which is looking at how profitable the strategy is on 
# historical data.Lets bring back Head AAPL

head(AAPL)
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")

AAPL_SMA_05 (
  Cl(AAPL),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 05     # The number of days in the moving average window
)

AAPL_SMA_10 <- SMA(
  Cl(AAPL),
  n = 10
)

AAPL_SMA_15 <- SMA(
  Cl(AAPL),
  n = 15
)

zoomChart("2016")  # Zoom into the year 2016 in the chart
addTA(AAPL_SMA_05, on = 1, col = "red")  # on = 1 plots the SMA with price
addTA(AAPL_SMA_10, on = 1, col = "blue")
addTA(AAPL_SMA_15, on = 1, col = "green")

# We will refer to the sign of this difference as the momentum; 
# that is, if the fast moving average is above the slow moving average, 
# this is a bullish momentum (the bulls rule), and a bearish momentum
# (the bears rule) holds when the fast moving average is below the slow moving 
# average. 
momentum(Cl(AAPL), n=5)

# The Price Rate of Change (PROC) is a momentum-based technical indicator 
# that measures the percentage change in price between the current price 
# and the price a certain number of periods ago. ... 
# The indicator can be used to spot divergences, overbought 
# and oversold conditions, and centerline crossovers.Breakout trading with the PROC
# Momentum oscillators such as the PROC indicator are very good at 
# trading ranges and breakouts. 


# This is because breakouts usually occur with strong momentum.
ROC(Cl(AAPL),n=05)

# Now, Lets Look At RSI
RSI(Cl(AAPL), n=05)

# Moving average convergence divergence (MACD) is a trend-following momentum 
# indicator that shows the relationship between two moving averages 
# of a security's price. ... Traders may buy the security when the MACD 
# crosses above its signal line and sell—or short—the security when the MACD 
# crosses below the signal line.
MACD(Cl(AAPL), nFast=12, nSlow=26,
     nSig=9, maType=SMA)

##### charting SMA
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addSMA(n=05,on=1,col = "blue")
addSMA(n=15,on=1,col = "red")

# CHARTING EMA 
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addEMA(n=20,on=1,col = "blue")
addEMA(n=100,on=1,col = "red")

# Charting Bollinger
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addBBands(n=20,sd=2)

## Charting Momentum
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addMomentum(n=1)

### Charting ROC 
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addROC(n=7)

## Charting MACD
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addMACD(fast=12,slow=26,signal=9,type="EMA")

### CHARTING RSI 
chartSeries(AAPL,
            subset='2021-01::2021-07',
            theme=chartTheme('white'))
addRSI(n=14,maType="EMA")

###Trading signals appear at momentum changes. When a bullish momentum begins, 
# a buy signal is triggered, and when it ends, a sell signal is triggered. 
# Likewise, when a bearish regime begins, a sell signal is triggered, and when 
# the regime ends, a buy signal is triggered (this is of interest only if you 
# ever will short the stock, or use some derivative like a stock option to bet 
# against the market).

### We would buy Apple stock 19 times and sell Apple stock 19 times. 
# If we only go long on Apple stock, only 19 trades will be engaged in over the 
# 6-year period, while if we pivot from a long to a short position every time a 
# long position is terminated, we would engage in 19 trades total. 
# (Bear in mind that trading more frequently isn’t necessarily good; 
# trades are never free.)

# You may notice that the system as it currently stands isn’t very robust, 
# since even a fleeting moment when the fast moving average is above the 
# slow moving average triggers a trade, resulting in trades that end immediately
# (which is bad if not simply because realistically every trade is 
# accompanied by a fee that can quickly erode earnings). 
# Additionally, every bullish regime immediately transitions into a 
# bearish regime, and if you were constructing trading systems that allow 
# both bullish and bearish bets, this would lead to the end of one trade 
# immediately triggering a new trade that bets on the market in the opposite 
# direction, which again seems finnicky. A better system would require more 
# evidence that the market is moving in some particular direction for or against
# the traders desired position.

## Charting Custom TA - NEED TO FIX THIS 
# SMA(Cl(AAPL),n=05)
# chartSeries(AAPL,
#           subset='2021-01::2021-07',
#           theme=chartTheme('white'))
# addTA(SMA, on=1, col="red")

# OK LETS BACK TEST PERFORMANCE 
install.packages("PerformanceAnalytics")
library (performanceAnalytics)
library(PerformanceAnalytics)

#### Before We evaluate training, we have to initiate trading signals. Recall that 
# Bull and Bear Markets are conditions that trigger our model. Now the Signal
# will be to buy or sell depending on the position our trader chooses. 
# 1. buy Signal based on RSI, 2. Buy signal based on EMA and sell Signal based on 
# RSI.



