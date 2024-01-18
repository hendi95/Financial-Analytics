####################### Case 2: CAPM: Step4-Step9 ###############################################

# Read the CSV file "Case2CAPM.csv" into the CAPM dataframe
CAPM <- read.csv("Case2CAPM.csv", header = TRUE, sep = ",")

# Get the dimensions of the CAPM dataframe (number of rows and columns)
dim(CAPM)

# Get the column names of the CAPM dataframe
names(CAPM)

# Display the CAPM dataframe in a viewer window
View(CAPM)

# Change the class of variables DATE to be "Date"
DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")

# Create the excess returns of IBM from 1994 to 2015
ibmRET <- CAPM$IBMRET
RF <- CAPM$RF
IBMEXERT <- ibmRET - RF

# Perform a linear regression of IBMEXERT on MarketEXRET
Model <- lm(IBMEXERT ~ CAPM$MarketEXRET)

# Print the summary of the linear regression model
Model

# Get the class of the Model object
class(Model)

# Get the names of variables in the Model object
names(Model)

# Get the formula and coefficients of the Model
Model$call
Model$coefficients

# Get access to the intercept and slope coefficients
Model$coefficients[1]
Model$coefficients[2]

# Create a summary of the model
Modelsum <- summary(Model)

# Get the class of the Modelsum object
class(Modelsum)

# Get the names of variables in the Modelsum object
names(Modelsum)

# Print the summary of the linear regression model
Modelsum

# Get the coefficient panel of the Modelsum object
Modelsum$coefficients

# Predict values using the linear regression model
mdlpred <- predict(Model)

# Get the length of the prediction results
length(mdlpred)

# Calculate the residuals of the model
mdlresid <- resid(Model)

# Get the length of the residuals
length(mdlresid)

# Save a scatter plot of MarketEXRET vs. IBMEXERT with the regression line to a JPEG file
jpeg(filename = "Case2_OLSLINE.jpeg")
plot(CAPM$MarketEXRET, IBMEXERT,
     main = "Scatter Plot of IBM Excess returns Vs. Market Excess returns",
     xlab = "Market Excess returns", ylab = "IBM Excess returns")
abline(Model, col = "blue")
dev.off()

# Is the adjusted returns zero?

# Step 1: According to the null hypothesis
testValue <- 0
n <- length(CAPM$MarketEXRET)

# Step 2: Compute the test statistics
estcoeff <- Modelsum$coefficients[1]
eststd <- Modelsum$coefficients[1, 2]
tstats <- (estcoeff - testValue) / eststd

# or use the simpler method
tstats <- Modelsum$coefficients[1, 3]

# Step 3: Define the decision rule for a two-sided test
decRule <- abs(tstats) > qt(1 - 0.05/2, n - 1 - 1)
decRule

# Step 4: Make a conclusion based on the decision rule
Result <- ifelse(decRule, "Reject", "Can't Reject")
Result

# Is the sensitivity higher than one?

# Step 1: According to the null hypothesis
testValue <- 1

# Step 2: Compute the test statistics
estcoeff <- Modelsum$coefficients[2]
eststd <- Modelsum$coefficients[2, 2]
tstats <- (estcoeff - testValue) / eststd

# Step 3: Define the decision rule for a two-sided test
decRule <- tstats > qt(1 - 0.05, n - 1 - 1)
decRule

# Step 4: Make a conclusion based on the decision rule
Result <- ifelse(decRule, "Reject", "Can't Reject")
Result



##### QUESTION 1 ######

# Consider the estimation of the beta coefficient from an ordinary least squares regression:

# Ri,t - Rf,t = alphai + betai (Rm,t+1 - Rf,t) + Ei,t+1

# The observations of this regressions are two time-series of daily excess returns. 
# The dependent (y) variable is _____individual excess returns______ and the independent variable(x)  
# is the ______market excess returns_______. 

# In this regression, the _____beta_______ is the ratio of the covariance to the variance of 
# the market excess returns. The ______alpha______ is the intercept in the regression. 
# The second term is the product of ______beta_______ and market excess return.

# This regression assumes that individual excess returns are linearly related to _____market excess returns______. 
# The _____idiosyncratic event______ is serially independent and it is normally distributed with a constant variance.

#### All Answer Choices
# individual excess returns
# market excess returns
# alpha
# beta
# non-diversifiable risk
# idiosyncratic event


##### QUESTION 2 ######

# Let yi,t+1 = Ri,t+1 - Rf,t,   Xi,t+1 = Rm,t+1 - Rf,t and the residuals are the leftover variation in the
# data after accounting for the model fit, 

#   ^                                             ^
# yi,t = alphai + biXi,t+1;   ei,t+1 = yi,t+1 - yi,t+1

#### Note a: Why do we change the notations here?

# A hat on y is used to signify that this is an _________estimate(s)/estimated__________ of the 
# individual excess returns at time t+1. It also means that this estimate is for an 
# expectation of the excess returns.

# The roman letters (a and b) signify that these are the _________estimate(s)/estimated___________ 
# of the population parameters alpha and beta in equation 3.2.

#### Note b: How many fitted values and residuals do we have?
  
# Since the number of observations in the regression is the number of daily excess returns. 
# We have ________5540__________ fitted values and __________5540_________ residuals.

#### Note c: How large is a residual?
  
# Each observation will have a residual. If an observation is above the regression line, then its residual, 
# the __________vertical___________ distance from the observation to the line, is _________positive_______. 
# Observations below the line have _______negative________ residuals.

#### Note d: How to find the best line?
  
# The line that minimizes this least-squares criterion is commonly called the least-squares line. 
# It is the most commonly used method. It is easy by hand and in most statistical software. 
# A residual twice as large as another residual is more than twice as bad. 
# Squaring the residuals accounts for this discrepancy.

# This means 
# 1: a residual "-5" is ______the same bad as_________  a residual "5"
# 2:  a residual "-5" is __________more than twice as bad as__________  a residual "-2.5"
# 3:  a residual "0.5" is ________more than twice as bad as__________ a residual "0.25"

##### All Answer Choices
# estimate(s)/estimated
# 5540
# 1
# vertical
# horizontal
# positive
# negative
# more than twice as bad as
# the same bad as
# less than twice as bad as


##### QUESTION 3 ######

# We run the following code
Model<-lm(IBMEXERT~marketEXERT)

# The variable " IBMEXERT" is the dependent variable on the left-hand side and the variable " marketEXERT" 
# is the only independent variable on the right-hand side. 

# ------------- TRUE


##### QUESTION 4 ######

# We run the following code
Model<-lm(IBMEXERT~marketEXERT)

# The output shows no intercept is estimated.

# ------------- FALSE

##### QUESTION 5 ######

# Intercept 0.04: The estimated daily average risk-adjusted excess return of 
# IBM (or the estimated daily alpha for IBM) is _______0.04%___________.

# Intercept SD 0.0196: If we choose another sample data of IBM and Market returns, 
# we will get a different estimated daily alpha. The estimates can be above 0.04% or below 0.04%. 
# On average, the variation away from 0.04% is ________0.0196%_____________.
  
# t-value is ______2.061_______: The coefficient t-value is a measure of how many standard deviations 
# our coefficient estimate is far away from zero. 

# p-value is ________3.94%_________: p-value is the probability of observing any value equal to or 
# larger than the absolute value of t. If the p-value of the intercept is less than 5%, 
# we can reject the null that alpha is insignificant.

##### All Answer Choices
# 0.04%
# 4%
# 0.0196%
# 1.96%
# 2.061
# 55.175
# 3.94%
# <2e-14%


##### QUESTION 6 ######

# Slope 0.91: For each additional 1% increase in the daily market excess return, we would expect an 
# increase of 0.91*1% = 0.91% in the daily excess return of IBM on average. The sensitivity beta is 
# estimated to be _______0.91__________.

# Slope SD 0.0165: If we choose another sample data of IBM and Market returns, we will get a different 
# estimated daily beta. The estimates can be above 0.91 or below 0.91. On average, the variation away 
# from 0.91 is _________0.0165________.

# t-value is _______55.175__________: The coefficient t-value is a measure of how many standard deviations 
# our coefficient estimate is far away from zero. 

# p-value is ________<2e-14%________: p-value is the probability of observing any value equal to or 
# larger than the absolute value of t. If the p-value of the slope is less than 5%, we can reject 
# the null that the linear relationship doesn't exist.

##### All Answer Choices
# 0.91
# 0.91%
# 0.0165
# 0.0165%
# 2.061
# 55.175
# 3.94%
# <2e-14%


##### QUESTION 7 ######

# We run the following code in R:
  
Model<-lm(IBMEXERT~marketEXERT)
mdlsum<-summary(Model)
mdlpred<-predict(Model)
mdlresid<-resid(Model)

##### Choices:	

# class of the object "Model" is 'lm'. It contains 12 variables.
# "Model$coefficients" reports estimates of intercept and slope. 
# class of the object "mdlsum" is "summary.lm". It contains 11 variables.
# The class of "mdlpred" and "mdlresid" are both numeric. But they have different lengths.

# ---------------------- The class of "mdlpred" and "mdlresid" are both numeric. But they have different lengths.

##### QUESTION 8 ######

# Residual Standard Error is a measure of the quality of a linear regression fit. 
# The Residual Standard Error is the average amount that the response will deviate from the regression line. 
# In our example, the actual IBM excess returns can deviate from the regression line 
# by approximately ______1.459%_______ , on average.

# It's also worth noting that the Residual Standard Error was calculated with _______5538_______ degrees of freedom. 
# Simplistically, degrees of freedom are the number of data points that went into the estimation of the 
# parameters used after taking into account these parameters (restriction). 
# In our case, we had ______5540________ data points and two parameters (intercept and slope).

##### All Answer Choices
# 1.459%
# 1.459
# 5540
# 5538


##### QUESTION 9 ######

# The R-squared (R2) statistic provides a measure of how well the model is fitting the actual data. 
# It takes the form of a proportion of variance. R2 always lies between _____0______ and _______1________. 
# A number near ________0_______ represents a regression that does not explain the variance in the response 
# variable well. And a number close to _______1_______ does explain the observed variance in the response 
# variable. In our example, the R2 we get is 0.3547. Or roughly _______35%_______ of the variance found in the 
# response variable (IBM excess returns) can be explained by the predictor variable (market excess returns).

##### All Answer Choices
# -1
# -0.5
# 0
# 1
# 0.35%
# 35%

##### QUESTION 10 ######


jpeg(filename = "Case1_OLSLINE.jpeg")

plot(marketEXERT, IBMEXERT,main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",xlab= "Market Excess returns", ylab="IBM Excess returns")

abline(lm(IBMEXERT~marketEXERT), col="blue")

dev.off()

# Which of the following is wrong?

# A. It plots market excess returns on the y-axis and IBM excess returns on the x-axis.

# B. abline(a=intercept value,b=slope value) usually requires at least two arguments. 
# In this case, once we put in an object of class "lm", it can also smartly extract the intercept 
# and the slope from the regression results.

# C. By default, the plot() function plots data as "circles". You can change its type by specifying 
# the value of the argument "type". For scatterplots, we don't use type="l" because connecting the dots 
# with lines doesn't make sense at all. 

# D. The label of y-axis is " IBM Excess returns" and the label of x-axis is " Market Excess returns ". 
# Both axes are correctly labeled. 

# --------------------- A

##### QUESTION 11 ######

# Ri,t+1 - Rf,t = alphai + betai(Rm,t+1 - Rf,t) + Ei,t+1

# Which of the following is wrong?

# A. Even if CAPM doesn't hold, the nonmarket/idiosyncratic risk can't be priced--- 
# the variance of idiosyncratic risk is uncorrelated with the intercept across different firms. 

# B. This is not the CAPM equation unless the intercept is zero and the expectation of idiosyncratic risk is zero.

# C. If CAPM holds, this equation suggests that the higher the beta, the higher expected returns.

# D. If CAPM holds, the beta risk is referred to in some textbooks as systematic or non-diversifiable 
# or market risk. This is the only risk rewarded with expected returns.

# --------------- A

##### QUESTION 12 ######

testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
tstats<-(Model$coefficients[1]-testValue)/summary(Model)[["coefficients"]][1,2]
Result<-ifelse(abs(tstats)>qt(0.975, length(marketEXERT)-1-1),"Reject", "Can't Reject")

# Which of the following is wrong?

# A. Why the test value is zero? Because the null says the risk-adjusted excess return is zero.

# B. tstats is computed according to equation 2.20. It is not the same as 
# " summary(Model)[["coefficients"]][1,3]  "

# C. qt(p, df=n-k-1) is the quantile function for the t distribution with degree of freedom equal to n-k-1. 
# This function can output the cutoff point that p of the elements within t distribution is lower 
# than the cutoff point. That is why we put in 0.975=1-5%/2 as the value for the first argument. 
# Here the number of observation n is 5540, the number of regressor k is 1. So the degree of freedom is 5538.

# D. Why do we use the absolute value of t? This is because it is a two-sided test. 
# An evident positive or negative t-stats both indicate a significant risk-adjusted return in the data. 
# Therefore, in either case, we will reject the null. Alternatively, we can write the following
# Result<-ifelse(tstats>qt(0.975, length(marketEXERT)-1-1) |  tstats<qt(0.025, length(marketEXERT)-1-1),  "Reject", "Can't Reject")
# qt(0.025, length(marketEXERT)-1-1) is 2.5% quantile from the t distribution with degree of freedom equal to 5538. It is -1.960392.

# ------------------ B

##### QUESTION 13 ######

testValue<-1
Model2<-lm(IBMEXERT~marketEXERT)
tstats2<-(Model2$coefficients[2]-testValue)/summary(Model2)[["coefficients"]][2,2]
Result<-ifelse(tstats2>qt(0.95, length(marketEXERT)),"Reject", "Can't Reject")

# Which of the following is wrong?

# A. Why the test value is one? Because the null says the risk exposure is less than or equal to one.
 
# B. tstats is computed according to equation 2.20. It is the same as 
# " summary(Model)[["coefficients"]][2,3]  "

# C. We put in 0.95=1-5% as the value for the first argument in qt(p, df=n-k-1) .  
# This is because it is a one-sided test. If we observe an evident positive t-stats, it indicates 
# that exposure to market risk is higher than one.

# D. The result is "can't reject"

# ------------------ B
