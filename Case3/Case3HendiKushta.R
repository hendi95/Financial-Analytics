# Load the data
Market<-read.csv("Case3Market.csv", header = FALSE, sep=",")

# dimension and names of variables
dim(Market)
names(Market)
View(Market)

# Change the class of variables DATE to be "Date"
DATE<-as.Date(as.character(Market$V1),"%Y%m%d")


#Load the data
Port<-read.csv("Case3Port.csv", header = FALSE, sep=",")

# dimension and names of variables
dim(Port) # 5025 days 6414 companies/firms
names(Port)
View(Port)

#Load the data
FirmSECID<-read.csv("Case3FirmSECID.csv ", header = TRUE, sep=",")

# dimension and names of variables
dim(FirmSECID) # 
names(FirmSECID) # security id, permanent code, ticker(Akronim), name of the company
View(FirmSECID)

##Step2: Summary Statistics Risk free rate and market excess return
Market_EXERT<-Market$V2 #5025
Rf<-Market$V3 #5025

## identify companies
SECID<-FirmSECID$secid
PERMNO<-FirmSECID$permno
TICKER<-as.character(FirmSECID$Ticker)
COMNAM<-FirmSECID$Name




## Step1: Drop Firms with no observations
ncol_Port<-ncol(Port) #6414
nrow_Port<-nrow(Port) #5025

## Create a variable to store the number of Nan for each column in Port data file
num_Nan<-rep(0, ncol_Port) # 6414

## calculate the number of Nan for each column in Port data file
for(i in 1:ncol_Port){
  num_Nan[i]<-sum (is.nan(Port[, i]))
}

## Find the firms that have no observations in the whole sample period
vec_DELETE <- which(num_Nan == nrow_Port) #1640, 2331, 2624
## Delete the firms that have no observations in the whole sample period
## TICKER[-c(i)] can return vector TICKER without ith element of TICKER,
## given i is a positive integer and is less than the length of TICKER.
SECID_Clean<-SECID[-c(vec_DELETE)]
PERMNO_Clean<-PERMNO[-c(vec_DELETE)]
TICKER_Clean<-TICKER[-c(vec_DELETE)]
COMNAM_Clean<-COMNAM[-c(vec_DELETE)]
Port[,vec_DELETE] <- NULL #5025 6411

## output the beginning and the ending date in our sample
lg<-length(DATE)
DATE[1]
DATE[lg]

## The number of firms that are kept and dropped
ncol_Port_Clean<-ncol(Port)
num_Dropped<-ncol_Port-ncol_Port_Clean

## Calculate Firm excess returns
Firm_RET<-Port-Rf #5025 6411

## Calculate descriptive statistics, such as mean, standard deviation, and Sharpe-ratio
# Firm_Mean<-apply(Firm_RET, 2, mean, na.rm=TRUE)


# Mean_Quantile reports the 5%, 25%, 50%, 75% and 95% of the average excess
# returns among different firms. Remove the Nan in the mean excess returns before
# taking the quantiles.
# Quantile_Percent<-c(0.05, 0.25, 0.5, 0.75, 0.95)
# Mean_Quantile<-quantile(Firm_Mean, Quantile_Percent, na.rm=TRUE)

# Mean_Quantile


## Step3.0 Drop Firms with at least one missing value
# ncol_Port<-ncol(Port) #6411
# nrow_Port<-nrow(Port) #5025



library(e1071)
## descriptive statistics
Firm_Mean<-apply(Firm_RET, 2, mean, na.rm=TRUE)
Firm_Std<-apply(Firm_RET, 2, sd, na.rm=TRUE)
Firm_Skew<-apply(Firm_RET, 2, skewness, na.rm=TRUE)
Firm_Kurt<-apply(Firm_RET, 2, kurtosis, na.rm=TRUE)
Firm_Min<-apply(Firm_RET, 2, min, na.rm=TRUE)
Firm_Max<-apply(Firm_RET, 2, max, na.rm=TRUE)
Firm_Sharpe<-Firm_Mean/Firm_Std*sqrt(252)

## Calculate descriptive statistics, such as mean, standard deviation, and Sharpe-ratio for IBM
IBM<-Firm_RET[, which(SECID_Clean==106276)]
IBM_Mean<-mean(IBM, na.rm = TRUE)
IBM_Std<-sd(IBM, na.rm = TRUE)
IBM_Skew<-skewness(IBM, na.rm = TRUE)
IBM_Kurt<-kurtosis(IBM, na.rm = TRUE)
IBM_Min<-min(IBM, na.rm = TRUE)
IBM_Max<-max(IBM, na.rm = TRUE)
IBM_Sharpe<-IBM_Mean/IBM_Std*sqrt(252)

## Calculate Quantile for each descriptive statistics
Quantile_Percent<-c(0.05, 0.25, 0.5, 0.75, 0.95)
Mean_Quantile<-quantile(Firm_Mean, Quantile_Percent, na.rm=TRUE)
Std_Quantile<-quantile(Firm_Std, Quantile_Percent, na.rm=TRUE)
Skew_Quantile<-quantile(Firm_Skew, Quantile_Percent, na.rm=TRUE)
Kurt_Quantile<-quantile(Firm_Kurt, Quantile_Percent, na.rm=TRUE)
Min_Quantile<-quantile(Firm_Min, Quantile_Percent, na.rm=TRUE)
Max_Quantile<-quantile(Firm_Max, Quantile_Percent, na.rm=TRUE)
Sharpe_Quantile<-quantile(Firm_Sharpe, Quantile_Percent, na.rm=TRUE)


## Construct a table to present the results
Table_2_1<-matrix(data=NA,nrow = 7, ncol = 6)
Table_2_1[1,]<-c(IBM_Mean, Mean_Quantile)
Table_2_1[2,]<-c(IBM_Std,Std_Quantile)
Table_2_1[3,]<-c(IBM_Skew,Skew_Quantile)
Table_2_1[4,]<-c(IBM_Kurt,Kurt_Quantile)
Table_2_1[5,]<-c(IBM_Min,Min_Quantile)
Table_2_1[6,]<-c(IBM_Max,Max_Quantile)
Table_2_1[7,]<-c(IBM_Sharpe,Sharpe_Quantile)
rownames(Table_2_1)<-c("Mean", "Std", "Skew", "Kurt", "Min", "Max", "Sharpe-Ratio")
colnames(Table_2_1)<-c("IBM","Q5","Q25","Q50","Q75","Q95")
as.table(round(Table_2_1,2))


## Firm identification
TICK_vec<-rep(NaN,4)
TICK_vec[1]<-which(SECID_Clean==106276) # IBM
TICK_vec[2]<-which(Firm_Sharpe==min(Firm_Sharpe, na.rm = TRUE))
TICK_vec[3]<-which((Firm_Sharpe==quantile(Firm_Sharpe, c(0.50), na.rm = TRUE)))
TICK_vec[4]<-which(Firm_Sharpe==max(Firm_Sharpe, na.rm = TRUE))

## Study these firms
Q_Index<-list() # row numbers that have observable return values
QLength<-integer() # number of days that have observable return values
QStarting<-character() # starting date
QEnding<-character() # ending date


for (i in 1:length(TICK_vec)){
  Q_Index[[i]]<-which(is.nan(Firm_RET[,TICK_vec[i]])== "FALSE")
  QLength[i]<-length(Q_Index[[i]])
  QStarting[i]<-Market$V1[Q_Index[[i]][1]]
  QEnding[i]<-Market$V1[Q_Index[[i]][QLength[i]]]
}

## Construct table to present previous results
Table_2_2<-matrix(data=NA,nrow =4, ncol = 4)
Table_2_2[1,]<-round(Firm_Sharpe[TICK_vec],4)
Table_2_2[2,]<-QLength
Table_2_2[3,]<-QStarting
Table_2_2[4,]<-QEnding
rownames(Table_2_2)<-c("Sharpe-Ratio", "Length", "Start", "End")
colnames(Table_2_2)<-c("IBM","Min","Q50","Max")
as.table(Table_2_2)




## Step 3 Beta Estimation
## Step 3.0 Drop Firms
## Create a variable to store the number of Nan for firm
Firm_num_Nan<-rep(0, ncol(Firm_RET))
## calculate the number of Nan for each firm
for(i in 1:ncol(Firm_RET)){
  Firm_num_Nan[i]<-sum (ifelse(is.nan(Firm_RET[, i]), 1, 0))
}
## Find firms that have incomplete sample length
Firm_DELETE<-which(Firm_num_Nan>0)
## Drop the firms with incomplete information during the sample period
SECID_DClean<-SECID_Clean[-c(Firm_DELETE)]
PERMNO_DClean<-PERMNO_Clean[-c(Firm_DELETE)]
TICKER_DClean<-TICKER_Clean[-c(Firm_DELETE)]
COMNAM_DClean<-COMNAM_Clean[-c(Firm_DELETE)]
Firm_DRET<-Firm_RET
Firm_DRET[, Firm_DELETE]<-NULL

## Calculate the number of firms after delete firms with incompelete observations during the sample period
Num_Firms<-ncol(Firm_DRET)
## The number of firms that are dropped
Num_Delete<-ncol_Port_Clean-Num_Firms
## The length, beginning date, and ending date
nrow(Firm_DRET)
DATE[1]
DATE[nrow(Firm_DRET)]





## Step 3.1 Unique Month
## Find the the specific dates for the end of each month in the sample period.
End_Month<-tapply(as.character(DATE), substr(DATE, 1, 7), max)
## Create a variable to store the index for the end of each month
End_Month_Index<-rep(NA, length(End_Month))
## Find the row number for the end of each month
for( i in 1:length(End_Month)){
  End_Month_Index[i]<-which(DATE==End_Month[i])
}


## 3.2 OLS regression firm by firm
Num_Month<-length(End_Month_Index)
Win<-60
Starting_Month_Index<-3
Beta_Estimation_3_2<-matrix(NA, nrow = 240, ncol=1111)
## Firm Beta Estimation
for (i in 1:Num_Firms){
  for(j in Starting_Month_Index:Num_Month){
    y<-Firm_DRET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i]
    x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
    Model<-lm(y~x)
    Beta_Estimation_3_2[j,i]<-Model$coefficients[2]
  }
}



## Step 4.1 Construct Portfolios at the end of each month
Month_RET_4<-matrix(NA, nrow =Num_Month, ncol =ncol(Firm_DRET))
Port_RET_Q<-matrix(NA, nrow =Num_Month, ncol =6)
for (j in Starting_Month_Index:(Num_Month-1)){
  for (i in 1:Num_Firms){
    Month_RET_4[j,i]<-sum(Firm_DRET[(End_Month_Index[j]+1):End_Month_Index[(j+1)], i],
                          na.rm=TRUE)
  }
  ## use beta quantiles as cutoff points
  cutoff<-quantile(Beta_Estimation_3_2[j,], c(0, 0.2, 0.4, 0.6,0.8,1), na.rm=TRUE)
  ## form portfolios at the end of each month
  Port_RET_Q[j,1]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>=cutoff[1])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[2]))])
  Port_RET_Q[j,2]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[2])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[3]))])
  Port_RET_Q[j,3]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[3])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[4]))])
  Port_RET_Q[j,4]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[4])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[5]))])
  Port_RET_Q[j,5]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[5])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[6]))])
  ## Return difference between highest quintile and lowest quintile
  Port_RET_Q[j,6]<-Port_RET_Q[j,5]-Port_RET_Q[j,1]
}


## Step 4.2 Portfolio Returns
## Calculate the time series average for each portfolio in each quantile
Port_RET_QMean<-apply(Port_RET_Q,2,mean, na.rm = TRUE)
Port_RET_Qsd<-apply(Port_RET_Q,2,sd, na.rm = TRUE)
Port_RET_QSharpe<-Port_RET_QMean/Port_RET_Qsd*sqrt(12)
## Create a Barplot for these five time-series averages and Sharpe Ratio
jpeg(filename = "Case3_Portfolio_4_2.jpeg")
barplot(Port_RET_QMean, xlab="Quantile",space= 1.5,ylab=c("Average Returns"))
dev.off()


## compute the p-value for time-series mean
x_4<-rep(1,Num_Month)
lmSUM<-summary(lm(Port_RET_Q~0+x_4))
Port_RET_Q_pvalue<-rep(NA,6)
Port_RET_Q_pvalue[1]<-lmSUM[["Response Y1"]]$coefficients[1,4]
Port_RET_Q_pvalue[2]<-lmSUM[["Response Y2"]]$coefficients[1,4]
Port_RET_Q_pvalue[3]<-lmSUM[["Response Y3"]]$coefficients[1,4]
Port_RET_Q_pvalue[4]<-lmSUM[["Response Y4"]]$coefficients[1,4]
Port_RET_Q_pvalue[5]<-lmSUM[["Response Y5"]]$coefficients[1,4]
Port_RET_Q_pvalue[6]<-lmSUM[["Response Y6"]]$coefficients[1,4]


## Table output
Table_4_2<-matrix(data=NA,nrow =4, ncol = 6)
Table_4_2[1,]<-Port_RET_QMean
Table_4_2[2,]<-Port_RET_Qsd
Table_4_2[3,]<-Port_RET_Q_pvalue
Table_4_2[4,]<-Port_RET_QSharpe
rownames(Table_4_2)<-c("Mean","Std","p-value","Sharpe-Ratio")
colnames(Table_4_2)<-c("Q1","Q2","Q3","Q4","Q5","Q5-Q1")
as.table(round(Table_4_2,3))


# step1: set up null and alternative
# step2: compute t-stats
Test_Value_4_3<-lmSUM[["Response Y6"]]$coefficients[1,3]
# step3:
decisionRule<-Test_Value_4_3>qt(0.95,(Num_Month-Starting_Month_Index-1))
# Step4:
Test_Result_4_3<-ifelse(decisionRule, "reject", "can't reject")
Test_Result_4_3




















