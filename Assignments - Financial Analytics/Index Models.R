####################### Index Models (Chap 8.1, 8.2, 8.3) ###############################################

##### QUESTION 1 ######

# A portfolio management organization analyzes 60 stocks and constructs a mean-variance efficient portfolio 
# using only these 60 securities. How many estimates of expected returns, variances, 
# and covariances are needed to optimize this portfolio?

# ? estimates of expected returns --------- 60
# ? estimates of variances ---------- 60 
# ? estimates of covariances -------- 1770

(60 * (60-1))/2     #1770

##### QUESTION 2 ######

# A portfolio management organization analyzes 60 stocks and constructs a mean-variance efficient portfolio 
# using only these 60 securities. If one could safely assume that stock market returns 
# closely resemble a single-index structure, how many estimates would be needed?
  
# ? estimates of alphai ------------- 60
# ? estimates of betai -------------- 60
# ? estimates of sigma^2 * e(i) ----- 60
# ? estimates of E(rM - rf) --------- 1
# ? estimates of sigma^2M ----------- 1


##### QUESTION 3 ######

# The following are estimates for two stocks.

## Stock    Expected Return   Beta    Firm-Specific Standard Deviation
## A              13%         0.8               30%
## B              18%         1.2               40%

# The market index has a standard deviation of 22% and the risk-free rate is 8%. 
# What are the standard deviations of stocks A and B? Assume that risk-free rate is constant. 

# The standard deviation of stock A  is ?%. (Rounded into two decimals)
# The standard deviation of stock B is ?%. (Rounded into two decimals )

ER <- c(0.13, 0.18)
Beta <- c(0.8, 1.2)
firm_specific_std <- c(0.3, 0.4)
market_index <- 0.22

variancee <- Beta^2 * market_index^2 + firm_specific_std^2 
std <- sqrt(variancee)
std * 100


##### QUESTION 4 ######

# The following are estimates for two stocks.

## Stock    Expected Return   Beta    Firm-Specific Standard Deviation
## A              13%         0.8               30%
## B              18%         1.2               40%

# The market index has a standard deviation of 22% and the risk-free rate is 8%.  
# Suppose that we were to construct a portfolio with proportions:
  
# Stock A: .30
# Stock B: .45
# T-bills: .25

# Compute the nonsystematic standard deviation of the portfolio.
# The nonsystematic standard deviation of the portfolio is ?%. (Rounded into two decimals)

portofolio_proportions <- c(0.3, 0.45)
nonsys_std_A <- sum(portofolio_proportions^2 * firm_specific_std^2)
sqrt(nonsys_std_A) * 100


##### QUESTION 5 ######

# The data below describe a three-stock financial market that satisfies the single-index model.

## Stock    Captitalization   Beta    Mean Excess Return    Standard Deviation
##  A             $3,000      1.0           10%                     40%
##  B             $1,940      0.2           2%                      30% 
##  C             $1,360      1.7           17%                     50%

# The standard deviation of the market-index portfolio is 25%. (Rounded into four decimals)

# a. What is the mean excess return of the three-stock value-weighted portfolio? ?%
# b. What is the covariance between stock A and stock B? ? 
# c. What is the covariance between stock B and the market-index? ?

# d. Break down the variance of stock B into its systematic and firm-specific components.
# Systematic risk equals ?
# Firm-specific risk equals ?

stock <- c("A", "B", "C")
capitalization <- c(3000, 1940,1360)
beta <- c(1.0, 0.2,1.7)
mean_excess_return <- c(0.1, 0.02, 0.17)
std_deviation <- c(0.4, 0.3, 0.5)
std_deviation_market_index <- 0.25

mean_excess_return_three_stocks <-sum((capitalization/sum(capitalization))*mean_excess_return)
mean_excess_return_three_stocks * 100

# beta * beta * market risk
covariance_A_B <- beta[1]*beta[2] * std_deviation_market_index^2
covariance_A_B

covariance_B_market_index <- beta[2] * 1.0 * std_deviation_market_index^2
covariance_B_market_index

systematic_risk <-beta[2]^2*std_deviation_market_index^2
systematic_risk                

specific_risk <- std_deviation[2]^2 - systematic_risk
specific_risk                     


##### QUESTION 6 ######

# Consider the following two regression lines for stocks A and B in the following figure.
# The slope of stock B is steeper than that of stock A. Therefore, stock B has higher firm-specific risk.

# -------- FALSE

##### QUESTION 7 ######

# The slope of stock B is steeper than that of stock A. Therefore, stock B has greater market risk.

# -------- TRUE

##### QUESTION 8 ######

# The absolute value of intercept of stock B is larger than that of stock A. Therefore, stock B has higher alpha.

# -------- FALSE

##### QUESTION 9 ######

# Stock B has higher R^2. Therefore, stock B has higher correlation with the market.

# -------- TRUE
