####################### Optimal Risky Portfolios (Chap 7.1, 7.2, 7.3, 7.4) ###############################################

##### QUESTION 1 ######
# Q1 and Q2 are based on the following informations.

# Suppose the expected return of equity is ErE = 13%, the expected return of debt is ErD = 8%. 
# The standard deviation of equity is sigmaE = 20%, the standard deviation of debt is sigmaD = 12%. 
# Compute the portfolio opportunity set for the debt and equity funds when the correlation coefficient between them is p = 0.25.
# The global minimum-variance portfolio is constructed so that:

# wE  = ? (Rounded to 4 decimals)
# wD  = ? (Rounded to 4 decimals)

ErE = 0.13
ErD = 0.08
sigma_E = 0.2
sigma_D = 0.12

p = 0.25 # correlation coefficient

wD = (sigma_E^2 - p * sigma_E * sigma_D) / (sigma_D^2 + sigma_E^2 - 2*p * sigma_E * sigma_D)
wD
wE = 1 - wD
wE

##### QUESTION 2 ######

# Based on the results of the previous question, calculate the expected return and standard deviation of the portfolio.
# Expected return: ErP = ?% (Rounded to 2 decimals)
# Standard deviation: sigmaP = ?% (Rounded to 2 decimals)

ErP = wE * ErE + wD * ErD 
ErP * 100

sigma_P = wE^2 * sigma_E^2 + wD^2 * sigma_D^2 + 2 * wE * wD * sigma_E * sigma_D * p
sqrt(sigma_P) * 100


##### QUESTION 3 ######
# Q3 to Q6 is based on the following informations.

# The universe of available securities includes two risky stock funds, A and B, and T-bills. 
# The data for the universe are as follows:

#####       Expected Return      Standard Deviation
# A               10%                   20%
# B               30%                   60%
# T-bills         5%                    0%

# The correlation coefficients between funds A and B is -0.2
# The covariance between A and B is minus (-)_____ ?

sigma_A <- 0.2
ERA <- 0.1
sigma_B <- 0.6
ERB <- 0.3
sigma_T_bills <- 0
Erf <- 0.05
p = -0.2 # correlation coefficients

cov_A_B <- sigma_A * sigma_B * p
cov_A_B


##### QUESTION 4 ######
# Find the optimal risky portfolio, P, and its expected return and standard deviation.

# The proportion of A is wA = ? (Rounded to 4 decimals)
# The proportion of B is wB = ? (Rounded to 4 decimals)
# The expected return of this portfolio is ErP = ?% (Rounded to 2 decimals)
# The standard deviation of this portfolio is sigmaP = ?% (Rounded to 2 decimals)

ErA <- ERA - Erf # calculate the excess return first - risk premium
ErA

ErB <- ERB - Erf 
ErB

wA <- (ErA * sigma_B^2 - ErB * cov_A_B) / (ErA * sigma_B^2 + ErB * sigma_A^2 - (ErA + ErB)*cov_A_B)
wA
wB <- 1-wA
wB

ErP <- wA * ERA + wB * ERB 
ErP
ErP*100

sigmaP <- wA^2 * sigma_A^2 + wB^2 * sigma_B^2 + 2 * wA * wB * sigma_A * sigma_B * p # risk
sigmaP <- sqrt(sigmaP)
sigmaP * 100

##### QUESTION 5 ######

# Based on the results of previous questions, calculate the Sharpe Ratio of this portfolio.
# SP equals______ (Rounded to 4 decimals)

SP <- (ErP - Erf) / sigmaP
SP

##### QUESTION 6 ######

# Based on the results of previous questions, how much will an investor with risk aversion A = 5 
# invest in funds A and B and in T-bills.?

# The fraction of total wealth in A is ? % (Rounded to 2 decimals)
# The fraction of total wealth in B is ? % (Rounded to 2 decimals)
# The fraction of total wealth in T-bills is ? % (Rounded to 2 decimals)

A <- 5       # risk aversion
y0 <-  (ErP - Erf) / (sigmaP^2 * A)
y0

y0 * wA # fraction of total wealth in A
y0 * wB # fraction of total wealth in B

1-y0 # fraction of total wealth in T-bills

