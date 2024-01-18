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

