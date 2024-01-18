####################### Financial Assets (Chap 1.1,1.2,2.3,1.6) ############################################

##### QUESTION 1 ######

# Which of the following assets are real assets?
  
##### Answers:	
# A. A $5 bill
# B. Customer goodwill
# C. A college education
# D. Patent
# E. Lease obligations

# -------------- B, C, D

##### QUESTION 2 ######

# Financial assets can be categorized as fixed income (debt securities), equity and derivative instruments. 
# For debt and equity, which investments tend to be risker?

##### Answers:	
# A. Equity Investments
# B. Debt Securities Investments

# ----------------- A

##### QUESTION 3 ######

# If you buy 100 shares of IBM stock, to what are you entitled?

##### Answers:	
# A. Entitled to get paid before all other claimants such as the tax authorities, employees, suppliers, bondholders.
# B. Entitled to vote in any of IBM's stockholder meetings.
# C. Entitled to a prorated share of IBM's dividend payments.

# ---------------- B, C

##### QUESTION 4 ######

# If you pay $150 per share for one share of IBM stock, your potential gain is unlimited over the next year, 
# because IBM's stock price has no upper bound.

##### Answers:	
# True
# False

# ------------------- TRUE

##### QUESTION 5 ######

# If you pay $150 per share for one share of IBM stock, what is the most money you could 
# lose over the year? $_______

# ------------------ 150


####################### Asset Return (Chap 2.1, 2.2, 2.3, 5.1, 5.2, 5.3)#####################################

##### QUESTION 1 ######

# Given the price, P(T), of a treasury bond with $100 par value and maturity of T years, we can use the 
# formula below to calculate the total risk-free return available for a horizon of T years ( rfT ).

# rfT = (100/ PT) - 1

# To compare investments in different horizon, we can calculate the effective annual rate (EAR), 
# defined as the percentage increase in funds invested over 1-year horizon, by using the following 
# equation:
  
# 1 + EAR = (1 + rf(T))^1/T

# Annualized rates on short-term investments (by convention, T<1 year) often are reported using 
# simple rather compound interest. These are called annual percentage rates (APRs),

# APR = rf(T)/T
# Given the example for half-year and 1 year, please complete the form for 25 years and 30 years.

### Horizon T     Price P(T)      (100/p(T)) - 1       Effective Annual Rate(EAR)      Annual Percentage Rates (APR)
##  Half-year       $97.36        100/97.36 - 1=2.71%   (1+0.0271)^2 - 1 = 5.49%        2.71% * 2 = 5.42%      
##  1 year          $95.52        100/95.52 - 1         (1+0.0469)^1 - 1 = 4.69%        4.69% / 1 = 4.69%
##  25 years        $23.30            ?                           ?                             ?
##  30 years        $16.89            ?                           ?                             ?

Horizon_T <- c(1/2, 1, 25, 30)
P_T <- c(97.36, 95.52, 23.30, 16.89)
par_value <- 100

# Find risk free return
rf <- (par_value/P_T) - 1
rf*100 # to return in percentages

# Find Effective Annual Rate
EAR <- (1+rf)^(1/Horizon_T) - 1
EAR * 100

# Find Annual Percentage Rates
APR <- rf/Horizon_T
APR * 100


##### QUESTION 2 ######

# The rate of realized return an investor receives from buying a common stock and holding it for a 
# given period of time is equal to the cash dividends received plus the capital gain during 
# the holding period divided by the purchase price of the security. 

# Calculate the realized return over one trading day (h=1 trading day) and realized return over 
# the past week (h=5 trading days). Round to the two decimal places. 
# If the return is not available, enter NA.

### DATE                        Price      Dividend      Return(h=1)     Return(h=5)
##  Friday May 8th, 2020        100           5               ?               ?
#   Monday May 11th, 2020       103.55        5               ?               ?
#   Tuesday May 12th, 2020      96.67         3               ?               ?
#   Wednesday May 13th, 2020    102.88        5               ?               ?
#   Thursday  May 14th, 2020    110.87        2               ?               ?
#   Friday  May 15th, 2020      107.42        5               ?               ?

price <- c(100, 103.55, 96.67, 102.88, 110.87, 107.42)
dividend <- c(5,5,3,5,2,5)

price_lag <- c(NA, head(price,-1))
price_lag
dividend_for <- c(NA, tail(dividend,-1))
dividend_for

return_h1 <- (price+dividend_for)/price_lag
return_h1

p_return_h1 <- na.omit(return_h1-1)
p_return_h1 * 100 # e para eshte NA, Kjo eshte e sakta


return_h5 <- prod(na.omit(return_h1))-1
return_h5


####################### Risk and Reward (Chap 1.5, 2.4, 5.4, 5.5, 5.6, 5.7) #############################

##### QUESTION 1 ######

# Holding-Period Returns (HPR) is the return on an asset or portfolio over the whole period 
# during which it was held. It is defined as

# holding period returns = HPR = (ending price of a share - beginning price + cash dividend)/beginning price

### State of the market       Probability       Beggining Price       Ending Price        Dividend        HPR
##        Boom                    0.35                105                 140               10             ?
##        Normal                  0.3                 100                 110               5              ?
##        Recession               0.35                96                  80                0              ?

# The expectation of the HPR on stocks is ____ %. (Rounded to 2 decimals)

state_of_market <- c('boom', 'normal', 'recession')
probability <- c(0.35, 0.3, 0.35)
beginning_price <- c(105, 100, 96)
ending_price <- c(140, 110, 80)
dividend <- c(10, 5, 0)

HPR <- (ending_price - beginning_price + dividend)/beginning_price
HPR

# expectation of the HPR on stocks ----- Expected Return ---- 0.35 * hpr boom + 0.3 * hpr normal ...
ER <- sum(probability * HPR)
ER


##### QUESTION 2 ######

# You invest $27,000 in a corporate bond selling for $900 per $1,000 par value. 
# Over the coming year, the bond will pay interest of $75 per $1,000 of par value. 
# The price of the bond at the end of the year will depend on the level of interest rates prevailing at 
# that time. You construct the following scenario analysis.

# Your alternative investment is a T-bill that yields a sure rate of return of 5%. Calculate the HPR 
# for each scenario the expected rate of return, and the risk premium on your investment. (Rounded HPR to 2 decimals) 

### Interest Rates      Probability     Year-End Bond Prices      HPR     End-of-Year Value
##  Higher                  0.2                 $850               ?              ?
##  Unchanged               0.5                 $915               ?              ?
##  Lower                   0.3                 $985               ?              ?

# Based on the results above, the Risk Premium is _____%.

invest <- 27000
# bond selling for 900 per 1000 par value p0
p0 <- 900
# the bond will pay interest of 75 -- Dividend - dt
dt <- 75
# sure rate of return 5%
rf <- 0.05

probability <- c(0.2, 0.5, 0.3)
year_end_bond_price <- c(850, 915, 985)

# RT or return
HPR <- (year_end_bond_price - p0 + dt) / p0
HPR * 100

# end of year value - FV
FV <- invest * (1+HPR)
FV

# Calculate the weighted expected return
ER <- sum(probability * HPR)
ER
# or
ER1 <- weighted.mean(HPR, probability)
ER1

# Calculate the Risk Premium - right solution
risk_premium <- (ER - rf) * 100
risk_premium

# or - not right dont know why
sig2R <- weighted.mean((HPR-ER1)^2,probability)
sigR <- sqrt(sig2R)
sigR <- round(sigR, 4)
sigR*100


##### QUESTION 3 ######

# Based on the risk-return trade-off principle, higher-risk assets priced to offer [A] expected returns than lower-risk assets.

##### Answers:	

# Based on the risk-return trade-off principle, higher-risk assets priced to offer 
# ________higher________  expected returns than lower-risk assets.

##### Choices:
# higher
# lower


##### QUESTION 4 ######

# We measure the risk-reward as the difference between the expected HPR on the index stock fund and 
# the risk-free rate. The risk-reward is also called the risk premium.

# Given the risk premium of a given stock is 7%, the current risk-free rate is 3%, 
# what is the expected HPR is _______%

risk_premium <- 0.07
risk_free <- 0.03
expected_HPR <- risk_premium + risk_free
expected_HPR * 100

##### QUESTION 5 ######

# You invest $1 million at the beginning of 2020 in an S&P 500 stock-index fund. 
# If the rate of return in 2020 is -40%, what rate of return in 2021 will be necessary for your 
# portfolio to recover to its original value? _____% (Rounded to 1 decimal)

invest <- 1000000
rate_return_2020 = -0.4

# rate_return_2021 = ?
  
FV_2020 <- invest * (1+rate_return_2020)
FV_2020

FV_2021 = 1000000
PV_2021 <- FV_2020
rate_return_2021 <- (FV_2021/PV_2021) - 1
round(rate_return_2021,4) * 100


##### QUESTION 6 ######
# Use the annual returns for years 1-3 in the table below, 

### Year      Implicit Probability      HPR(Return)
##   1                1/3                 0.2869
##   2                1/3                 0.1088
##   3                1/3                 0.0491

# Compute the arithmetic average return. ______% (Rounded to 2 decimals)
# Compute the standard deviation of returns. ______% (Rounded to 2 decimals)
# Compute the Sharpe ratio, assuming the risk-free rate was 6% per year.  _______ (Rounded to 2 decimals)

year <- c(1,2,3)
implicit_probability <- c(1/3, 1/3, 1/3)
HPR <- c(0.2869, 0.1088, 0.0491)
rf <- 0.06

average_return = sum(HPR)/length(HPR)
average_return

standard_deviation_of_return <- sd(HPR)
standard_deviation_of_return
# or
standard_deviation_of_return <- sqrt(1/(length(HPR)-1) * sum((HPR - average_return)^2))
standard_deviation_of_return

sharpe_ratio <- (average_return-rf)/standard_deviation_of_return
sharpe_ratio


##### QUESTION 7 ######

# This and the next question are based on the following information.

# The value at risk (VaR) is the loss corresponding to a very low percentile of the entire return 
# distribution. It is another name of quantile of a distribution. Practitioners commonly estimate the 
# 5% VaR, meaning that 95% of the returns will exceed the VaR, and 5% of the returns will be worse.
# To obtain a sample estimate of VaR, we sort the observations from high to low. 
# The VaR is the 5% percentile of the sample distribution.
# Suppose a sample comprises 84 annual returns. 
# The bottom 6 returns are -50%, -40%, -35%, -30%, -25%, -20%. The VaR is _____%

nr_observations <- 84
percentile <- 0.05     #5%
returns <- c(-0.50, -0.40, -0.35, -0.30, -0.25, -0.20)
returns <- sort(returns)
returns

cutoff <- nr_observations * percentile
cutoff

# Extract the whole part and decimal part of the cutoff
whole_part <- floor(cutoff)
decimal_part <- cutoff - whole_part

# Calculate VaR
VaR <- returns[whole_part] * (1 - decimal_part) + returns[whole_part + 1] * decimal_part

# OR
VaR <- -0.3 * (1-0.2) + -0.25*0.2
VaR


##### QUESTION 8 ######

# The expected shortfall is a more informative view of downside exposure would focus instead on the 
# the expected loss given that we find ourselves in one of the worst-case scenarios.

# Extending the previous VaR example, we assume equal probabilities for all values. 
# Hence, we need to average across the bottom 5% of the observations. The expected shortfall is ______% (rounded to 2 decimals).


ES <- (sum(returns[1:whole_part]) + returns[whole_part + 1] * decimal_part) / cutoff
ES


##### QUESTION 9 ######

# Consider the data in the table below, calculate the percentage change in the market-value-weighted index for the portfolio.

### Stock     Initial Price     Final Price     Shares      Initial Value of Outstanding Stock
##   ABC          $25               $20           20                        $500
##   XYZ          $100              $110          1                         $100

# The percentage change is ______%. (If it is a gain, the sign should be positive, if it is a loss, the sign should be negative)

initial_price <- c(25, 100)
final_price <- c(20, 110)
shares <- c(20,1)
initial_value_of_outstanding_stock <- c(500, 100)

final_value_of_outstanding_stock <- final_price * shares
final_value_of_outstanding_stock

percentage_change <- sum(final_value_of_outstanding_stock) / sum(initial_value_of_outstanding_stock) - 1
percentage_change





####################### Case 1 Stock Returns #############################

CAPM <- read.csv("Case2CAPM.csv", header = TRUE, sep = ",")
View(CAPM)

dim(CAPM)
colnames(CAPM)
names(CAPM)

DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")


# create excess return for IBM
ibmRET <- CAPM$IBMRET
RF <- CAPM$RF
IBMEXERT <- ibmRET - RF

# Summary statistics for IBM excess returns
summary(IBMEXERT)

# Summary statistics for market excess returns
summary(CAPM$MarketEXRET)


# build the time series of IBM excess return (%)
jpeg(filename = "case1_IBMEXERT.jpeg")
plot(DATE, IBMEXERT,
     type = "l",
     xlab="year",
     ylab = "daily excess return(%)",
     main = "IBM Excess Return(%)",
     ylim = c(-15,15))
dev.off()

# build the time series of Market excess return (%)
jpeg(filename = "case1_MarketEXERT.jpeg")
plot(DATE, CAPM$MarketEXRET,
     type = "l",
     xlab="year",
     ylab = "daily excess return(%)",
     main = "Market Excess Return(%)",
     ylim = c(-15,15))
dev.off()

# boxplot of IBM excess return
jpeg(filename = "case1_IBMEXERT_Boxplot.jpeg")
boxplot(IBMEXERT,
        main="Boxplot of IBM Excess Return",
        ylab="daily excess return(%)")
dev.off()

# boxplot of Market excess return
jpeg(filename = "case1_MarketEXRET_Boxplot.jpeg")
boxplot(CAPM$MarketEXRET,
        main="Boxplot of Market Excess Return",
        ylab="daily excess return(%)")
dev.off()



# calculate the annualized mean return for IBMEXERT by multiplying the daily mean by 252 (assuming trading days).
ibmMean <- mean(IBMEXERT)*252
ibmMean
# calculate the annualized standard deviation for IBMEXERT by multiplying the daily standard deviation by the square root of 252.
ibmSTD <- sd(IBMEXERT)*sqrt(252)
ibmSTD
# calculate the Sharpe Ratio (risk-adjusted return) for IBMEXERT by dividing the annualized mean return by the annualized standard deviation.
ibmSR <- ibmMean/ibmSTD
ibmSR

# calculate the Value at Risk (VaR) for IBMEXERT at a 5% significance level.
ibmVaR <- quantile(IBMEXERT, probs = c(0.05))
ibmVaR

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# steps to find estimated shortfall
# convert IBMEXERT to raw percentages by dividing by 100.
IBMEXERT_raw<-IBMEXERT/100
# calculate the Estimated Shortfall (ES) for IBMEXERT_raw at a 5% significance level using the historical method.
IBMES_raw<-ES(IBMEXERT_raw, p=.05,method="historical")
# convert the ES result back to percentage format.
IBMES<- IBMES_raw*100
IBMES



# calculate the annualized mean return for MarketEXRET by multiplying the daily mean by 252 (assuming trading days).
MarketMean <- mean(CAPM$MarketEXRET)*252
MarketMean
# calculate the annualized standard deviation for MarketEXRET by multiplying the daily standard deviation by the square root of 252.
MarketSTD <- sd(CAPM$MarketEXRET)*sqrt(252)
MarketSTD
# calculate the Sharpe Ratio (risk-adjusted return) for MarketEXRET by dividing the annualized mean return by the annualized standard deviation.
MarketSR <- MarketMean/MarketSTD
MarketSR

# calculate the Value at Risk (VaR) for MarketEXRET at a 5% significance level.
MarketVaR <- quantile(CAPM$MarketEXRET, probs = c(0.05))
MarketVaR

# convert MarketEXRET to raw percentages by dividing by 100.
MarketEXERT_raw<-IBMEXERT/100
# calculate the Estimated Shortfall (ES) for MarketEXERT_raw at a 5% significance level using the historical method.
MarketEXERT_raw<-ES(MarketEXERT_raw, p=.05,method="historical")
# convert the ES result back to percentage format.
MarketMES<- MarketEXERT_raw*100
MarketMES


#install.packages("e1071")
library(e1071)

# calculate the skewness of IBMEXERT, which measures the asymmetry of its return distribution.
ibmSkew <- skewness(IBMEXERT)
ibmSkew
# calculate the kurtosis of IBMEXERT, which measures the tails and peakedness of its return distribution.
ibmKurto <- kurtosis(IBMEXERT)
ibmKurto

# calculate the skewness of the MarketEXRET, measuring the asymmetry of its return distribution.
MKTskew<-skewness(CAPM$MarketEXRET)
MKTskew
# calculate the kurtosis of MarketEXRET, which measures the tails and peakedness of its return distribution.
MKTkurto<-kurtosis(CAPM$MarketEXRET)
MKTkurto

# Calculate the correlation between IBMEXERT and MarketEXRET
IBMcMarket <- cor(IBMEXERT, CAPM$MarketEXRET)
IBMcMarket

# construct each column of our table.
# define a vector 'Name' with labels for various statistical measures.
Name<-c("Mean:", "Std:", "Skewness:", "Kurtosis:","Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )
# create a vector 'IBM' containing corresponding values for IBM statistics.
IBM<-c(ibmMean, ibmSTD, ibmSkew, ibmKurto, ibmSR, ibmVaR, IBMES, IBMcMarket)
# create a vector 'Market' containing corresponding values for Market statistics.
Market<-c(MarketMean, MarketSTD, MKTskew, MKTkurto, MarketSR,MarketVaR, MarketMES, NA)

# construct dataframe with Name as row names
data.frame(IBM, Market,row.names =Name,check.names = TRUE)


# histogram of IBM excess return
jpeg(filename = "case1_IBMEXERT_Histogram.jpeg")
hist(IBMEXERT,
     main="Daily IBM Excess returns(percentage)",
     prob =TRUE,
     xlab = "IBM Excess Return",
     ylab = "Density",ylim = c(0,0.25),
     breaks = 50)
dev.off()

# histogram of Market excess return
jpeg(filename = "case1_MarketEXRET_Histogram.jpeg")
hist(CAPM$MarketEXRET,
     main="Daily Market Excess returns(percentage)",
     prob =TRUE,
     xlab = "Market Excess Return",
     ylab = "Density",ylim = c(0,0.25),
     breaks = 50)
dev.off()

#install.packages("tseries")
library(tseries)
# perform the Jarque-Bera test on the IBMEXERT data to assess if it follows a normal distribution.
jarque.bera.test(IBMEXERT)


#install.packages("nortest")
library(nortest)
# perform the Lilliefors test on the IBMEXERT data to check for normality (goodness-of-fit test).
lillie.test(IBMEXERT)


# perform the Jarque-Bera test on the MarketEXRET data to assess if it follows a normal distribution.
jarque.bera.test(CAPM$MarketEXRET)

# perform the Lilliefors test on the MarketEXRET data to check for normality (goodness-of-fit test).
lillie.test(CAPM$MarketEXRET)


##### QUESTION 1 ######

# Histograms are especially convenient for describing the shape of the data distribution. When data
# trail off to the right and have a longer right tail, the shape is said to be right-skewed. Data sets
# with the reverse characteristic - a long, thin tail to the left - are said to be left-skewed. 
# Data sets that show roughly equal trailing off in both directions are called symmetric. 
# Higher bars in a histogram represent where the data are relatively larger in value.

#----------------- FALSE
