####################### Capital Allocation to Risk Assets (Chap 6) ###############################################

##### QUESTION 1 ######

# Consider a risky portfolio. The end-of-year cash flow derived from the portfolio will be either $70,000 or $200,000 with equal probabilities of 
# .5. The alternative risk-free investment in T-bills pays 6% per year. a. If you require a risk premium of 8%, how much will you be willing to pay for the portfolio? 

Expected_FV <- 70000 * 0.5 + 200000 * 0.5
Expected_FV

rf <- 0.06 # risk free
risk_premium <- 0.08

ER <- rf + risk_premium # Expected Return
ER

investment <- Expected_FV / (1+ER)
investment


##### QUESTION 2 ######

# Investment Management Inc. (IMI) uses the capital market line to make asset allocation recommendations. IMI derives the following forecasts:

#  ∙ Expected return on the market portfolio: 12% 
#  ∙ Standard deviation on the market portfolio: 20%
#  ∙ Risk-free rate: 5% 

# Samuel Johnson seeks IMI’s advice for a portfolio asset allocation. Johnson informs IMI that he wants the standard deviation of the portfolio to equal half 
# of the standard deviation for the market portfolio. Using the capital market line, what expected return can IMI provide subject to Johnson’s risk constraint? ____ % (Rounded to one decimal)

ER <- 0.12 # expected return of 12%
market_std <- 0.2 # market standard deviation 20%
rf <- 0.05 

portofolio_std <- market_std/2
portofolio_std

sharpe_ratio <- (ER - rf)/market_std
sharpe_ratio

ERc <- rf + sharpe_ratio * portofolio_std
ERc * 100



##### QUESTION 3 ######

# The following five questions are based on the following information.

# You manage a risky portfolio with an expected rate of return of 18% and a standard deviation of 28%. 
# The T-bill rate is 8%. Your client chooses to invest 70% of a portfolio in your fund and 30% in an essentially risk-free money market fund. 
# What is the standard deviation of the rate of return on his portfolio? ____% (Rounded to two decimals)

ER <- 0.18 #18% expected rate of return
portofolio_std <- 0.28 #28% stdc
rf <- 0.08 #t-bill rate 8%

y0 <- 0.7 # client chooses to invest 70% of a portofolio in your fund.

complete_portofolio_std <- portofolio_std * y0
complete_portofolio_std * 100


##### QUESTION 4 ######
# What is the reward-to-volatility (Sharpe) ratio (S) of your risky portfolio?(Rounded to 4 decimals）

sharpe_ratio <- (ER - rf) / portofolio_std
sharpe_ratio



##### QUESTION 5 ######
# Suppose that your risky portfolio includes the following investments in the given proportions:
#  Stock A 25%
#  Stock B 32%
#  Stock C 43%
# What are the investment proportions of your client’s overall portfolio, including the position in T-bills (30.0% T-bills)? (Rounded to two decimals)

risky_portofolio <- c(0.25, 0.32, 0.43)
investment_proportions <- risky_portofolio * y0
investment_proportions * 100



##### QUESTION 6 ###### ***************************************** CHECK THIS

# Your client’s degree of risk aversion is A = 3.5.
# What proportion, y, of the total investment should be invested in your fund? (Rounded to four decimals)

risk_aversion <- 3.5 # A
y0_star <- (ER - rf)/(risk_aversion*portofolio_std^2) 
y0_star


##### QUESTION 7 ######

# What is the expected value of the rate of return on your client’s optimized portfolio? (Rounded to two decimals) ____ %

ERc <- rf + y0_star * (ER - rf)
ERc * 100

# OR

ERc <- rf + sharpe_ratio * y0_star * portofolio_std
ERc * 100



##### QUESTION 8 ######

# You estimate that a passive portfolio, for example, one invested in a risky portfolio that mimics the S&P 500 stock index, 
# yields an expected rate of return of 13% with a standard deviation of 25%. You manage an active portfolio with expected return 18% and standard deviation 28%. 
# The risk-free rate is 8%. Consider again with A = 3.5. 

# a. If this client chose to invest in the passive portfolio as his risky portfolio, what proportion, y, would he select? ____ % (Rounded to two decimals)

ER <- 0.13
safe_std <- 0.25
rf <- 0.08
A <- 3.5

y0_star <- (ER - rf)/(A *safe_std^2)
y0_star * 100


##### QUESTION 9 ######

# Consider a portfolio that offers an expected rate of return of 12% and a standard deviation of 18%. 
# T-bills offer a risk-free 7% rate of return. What is the maximum level of risk aversion for which the risky portfolio is still preferred to T-bills?  A < _____ (Rounded to two decimals)

ER <- 0.12
std_p <- 0.18
rf <- 0.07

uf <- rf - 0
uf
# up <- ER - 0.5 * A * std_p^2        uf = up
A <- (ER - uf) / (0.5*std_p^2)
A

