####################### Capital Asset Pricing Model (CAPM) (Chap 9) ###############################################

##### QUESTION 1 ######

# Data from the last nine decades for the S&P 500 index yield the following statistics: average excess return, 
# 8.3%; standard deviation, 20.3%
# To the extent that these average approximated investor expectations for the period, what must 
# have been the average coefficient of risk aversion? 
# _
# A = ? (Rounded to 2 decimals)

# If the coefficient of the risk aversion were actually 3.5, what risk premium would have been consistent 
# with the market's historical standard deviation? 
# The risk premium is ?% (Rounded to 2 decimals)

average_excess_return <- 0.083
market_std_deviation <- 0.203

A = average_excess_return / market_std_deviation^2 
A

A <- 3.5
risk_premium <- A*market_std_deviation^2
risk_premium * 100

##### QUESTION 2 ######

# Capital asset pricing theory asserts that portfolio returns are best explained by:
  
##### Answers:	
# A. Diversification
# B. Economic factors
# C. Systematic Risk
# D. Specific Risk

# ------------- C

##### QUESTION 3 ######

# The security market line depicts:
  
##### Answers:	
# A. The complete portfolio as a combination of the market portfolio and the risk-free asset.
# B. The market portfolio as the optimal portfolio of risky securities.
# C. A security's required expected return as a function of its systematic risk.
# D. The relationship between a security's return and the return on an index.

# ---------------- C

##### QUESTION 4 ######

# Kaskin, Inc., stock has a beta of 1.2 and Quinn, Inc., stock has a beta of 0.6. Which of the following statements is most accurate?
  
##### Answers:	
# A. The fair expected rate of return will be higher for the stock of Kaskin, Inc. than that of Quinn, Inc.
# B. The stock of Quinn, Inc., has more systematic risk than the that of Kaisin, Inc.
# C. The stock of Kaisin, Inc., has more total risk than the stock of Quinn, Inc.

# ----------------- A

##### QUESTION 5 ######

# What is the fair expected rate of return for a stock that has a beta of 1.0 if the expected return on the market is 15%?
  
##### Answers:	
# A. More than 15%
# B. Cannot be determined without the risk-free rate
# C. 15%

# --------------- C

##### QUESTION 6 ######

# The risk-free rate is 8% and the expected return on the market portfolio is 16%. 
# A firm considers a project that is expected to have a beta of 1.3.

# What is the required rate of return on the project?_____%

rf <- 0.08
market_excess_return <- 0.16
beta <- 1.3

required_rate_return <- rf + beta*(market_excess_return - rf)
required_rate_return * 100

##### QUESTION 7 ######

# Suppose that the risk premium on the market portfolio is estimated at 8% with a standard deviation of 22%. 
# What is the required risk premium on a portfolio invested 25% in Toyota and 75% in Ford if they 
# have betas of 1.10 and 1.25, respectively?
  
# The required risk premium is_____%

risk_premium <- 0.08
market_std_deviation <- 0.22 # given just to mess up, not used.

toyota_inv <- 0.25
ford_inv <- 0.75

toyota_beta <- 1.10
ford_beta <- 1.25

portofolio_beta <- toyota_inv * toyota_beta + ford_inv * ford_beta

required_rate_return <- portofolio_beta * (risk_premium) 
required_rate_return * 100

##### QUESTION 8 ######

# The difference between the fair and actually expected rate of return on a stock is called the 
# stock's alpha, denoted by alpha.

# Stock XYZ has an expected return of 12% and risk of beta=1. Stock ABC has expected return 
# of 13% and beta=1.5. The market expected return is 11%, and rf = 5%.

# What is the alpha of XYZ? ?%
# What is the alpha of ABC? ?% 

xyz_expected_return <- 0.12
xyz_beta <- 1  

abc_expected_return <- 0.13  
abc_beta <- 1.5  

market_excess_return <- 0.11  
risk_free <- 0.05  

alfa_xyz <- (xyz_expected_return-risk_free) - xyz_beta * (market_excess_return - risk_free)
alfa_xyz * 100

alfa_abc <- (abc_expected_return-risk_free) - abc_beta * (market_excess_return - risk_free)
alfa_abc * 100

##### QUESTION 9 ######


# Based on the results in the previous question, which stock is a better buy?
  
##### Answers:	
# A. Stock XYZ
# B. Stock ABC

# --------------- A


