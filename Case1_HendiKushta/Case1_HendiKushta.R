
# load the data
CAPM <- read.csv("Case1CAPM.csv", header = TRUE, sep=",")

# check dataframe dimensions, names of the columns
dim(CAPM)
names(CAPM)

# view the data in tabular format.
View(CAPM)

# trasform DATE in date type
DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")

# create excess return for IBM
ibmRET <- CAPM$IBMRET
RF <- CAPM$RF
IBMEXERT <- ibmRET - RF

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




