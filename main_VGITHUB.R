# Load required Packages
# THe packages can be installed from below insall.packages() commands
#install.packages("package_name")
# 

#install.packages(c('plyr', 'lubridate', 'quantmod', 'curl', 'ggplot2',
#                   'plotly', 'DT', 'tseries',  'forecast',  'xts', 'e1071', 
#                   'doParallel', 'caret', 'tidyr', 'tseries', 'TSA', 'tibble', 'forecast', 
#                   'gridExtra', 'corrplot', 'data.table', 'TTR' ))


library(dplyr)
library(lubridate)
library(quantmod)
library(curl)
library(ggplot2)
library(plotly)
library(DT)
library(tseries)
library(forecast)
library(xts)
library(e1071)
library(doParallel)
library(caret)
library(tidyr)
library(tseries)
library(TSA)
library(tibble)
library(forecast)
library(gridExtra)
library(corrplot)
library(data.table)
library(TTR)
rm(list = ls())


################################
# Fetching Data from Yahoo API #
################################


# Find access to real time data, I would like to put in the stock tickers name and have the program download and interpret 
# 1yr worth of data. The data that can be downloaded online or is provided by certain packages includes : the price of the stock, 
# the high of day, the low , the closing price, the opening price, and the volume of the day. 


start <- as.Date("2012-01-03")
end <- as.Date("2015-05-08")

# Let's get Google stock data; Google's ticker symbol is GOOG. We use the quantmod function getSymbols, and pass a string as a first argument to
# identify the desired ticker symbol, pass 'yahoo' to src for Yahoo! Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the global environment, with the object being named after the loaded ticker
# symbol. This feature may become deprecated in the future, but we exploit it now.

getSymbols("GOOG", src = "yahoo", from = start, to = end)

head(GOOG, n = 20)

## Plot some visualizations
plot(GOOG[, "GOOG.Close"], main = "GOOG")


# View Data Frame
View(GOOG)

# Plot Open Prices
ggplot(GOOG, aes(x = index(GOOG), y = GOOG[,1])) + geom_line(color = "red") + 
  ggtitle("Google Open Prices") + xlab("Date") + ylab("Open Prices") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

# Plot Adjusted Close Prices
ggplot(GOOG, aes(x = index(GOOG), y = GOOG[,6])) + geom_line(color = "darkblue") + 
  ggtitle("Google Adjusted Close Prices") + xlab("Date") + ylab("Adjusted Close Prices") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months")



# Plot Histogram for OHLC
options(repr.plot.width=10, repr.plot.height=10) 
p1 = ggplot(GOOG, aes(GOOG.Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p2 = ggplot(GOOG, aes(GOOG.High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p3 = ggplot(GOOG, aes(GOOG.Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p4 = ggplot(GOOG, aes(GOOG.Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)




# Candle Stick Chart for 
candleChart(GOOG, up.col ="black", dn.col = "red", theme = "white")
chartSeries(GOOG, subset='last 3 months')

# Add Technical Indicators from Quantmod
addADX()
addATR()
addBBands()
addCCI()
addCMF()
addCMO()
addEMA()
addEnvelope()
addEVWMA()
addMACD()
addMomentum()
addExpiry()
addSAR()
addSMA()
addSMI()
addDEMA()
addDPO()
addROC()
addRSI()
addVo()
addWMA()
addWPR()
addZLEMA()



# Calculate Technical Indicators for our own dataset
# Using the data which can be found online, we need to calculate the variables which are of interest, 
# and that will be analyzed to interpret success of a trade. 


# Average Volume  - 10day, 20day , 60day 
# This is the average volume of the past 10/20/60 days. 

GOOG$Avg_vol_10  <- SMA(GOOG$GOOG.Volume, n = 10)
GOOG$Avg_vol_20  <- SMA(GOOG$GOOG.Volume, n = 20)
GOOG$Avg_vol_60  <- SMA(GOOG$GOOG.Volume, n = 60)

# VolumeAverage%  - 10day. 20day, 60day
# This is the % of the average volume it has done on a particular day
GOOG$Volm_avg_pct_10 <- (GOOG$GOOG.Volume/GOOG$Avg_vol_10)*100
GOOG$Volm_avg_pct_20 <- (GOOG$GOOG.Volume/GOOG$Avg_vol_20)*100
GOOG$Volm_avg_pct_60 <- (GOOG$GOOG.Volume/GOOG$Avg_vol_60)*100

# Range 
# Todays high low 
GOOG$Range <- GOOG$GOOG.High - GOOG$GOOG.Low 

# Change% 
# The percentage change from yesterdays closing  price and todays closing price. 
GOOG$NetChange_pct <- (GOOG$GOOG.Close - lag(GOOG$GOOG.Close))/lag(GOOG$GOOG.Close) * 100

# Net Change
# The difference between the prior days closing price and todays closing price
GOOG$NetChange <- GOOG$GOOG.Close - lag(GOOG$GOOG.Close)


# Average Range 10day, 20day, 60day
# The average of the ranges over the last 10,20,60 days. 
GOOG$Avg_range_10 <- SMA(GOOG$Range, n = 10)
GOOG$Avg_range_20 <- SMA(GOOG$Range, n = 20)
GOOG$Avg_range_60 <- SMA(GOOG$Range, n = 60)


# Average Range % - 10day, 20day, 60day 
# The percentage of the Average Range 10/20.60 day  It has done today
GOOG$Avg_range_pct_10 <- (GOOG$Range/GOOG$Avg_range_10) * 100
GOOG$Avg_range_pct_20 <- (GOOG$Range/GOOG$Avg_range_20) * 100
GOOG$Avg_range_pct_60 <- (GOOG$Range/GOOG$Avg_range_60) * 100


# Dollar Volume
# The amount of money traded in the stock today Price x Volume 
GOOG$Dollar_volume <- GOOG$GOOG.Close*GOOG$GOOG.Volume


# Average Dollar Volume  10/20/60
# The average dollar volume over the last 10/20/60 days
GOOG$Avg_Dollar_volume_10 <- SMA(GOOG$Dollar_volume, n = 10)
GOOG$Avg_Dollar_volume_20 <- SMA(GOOG$Dollar_volume, n = 20)
GOOG$Avg_Dollar_volume_60 <- SMA(GOOG$Dollar_volume, n = 60)


# Average Dollar Volume % 
# The percentage of the average dollar volume the stock has done today.
GOOG$Avg_Dollar_volume_pct_10 <- (GOOG$Dollar_volume/GOOG$Avg_Dollar_volume_10) * 100
GOOG$Avg_Dollar_volume_pct_20 <- (GOOG$Dollar_volume/GOOG$Avg_Dollar_volume_20) * 100
GOOG$Avg_Dollar_volume_pct_60 <- (GOOG$Dollar_volume/GOOG$Avg_Dollar_volume_60) * 100

# Overnight Gap
# Todays open Yesterdays Close. 
GOOG$Overnight_gap <- GOOG$GOOG.Open - lag(GOOG$GOOG.Close)

# Overnight Gap % 
# The percentage gain or loss from yesterdays closing price
GOOG$Overnight_gap_pct <- (GOOG$GOOG.Open - lag(GOOG$GOOG.Close))/lag(GOOG$GOOG.Close) * 100


# Range%  at Close
# RANGE % - WHERE IT CLOSES RELATIVE TO THE HIGH AND LOW - EG. IF HIGH IS 100 AND LOW IS 0 , 
# IF IT CLOSES AT 50, IT HAS CLOSED AT 50% OF RANGE. THIS CHARACTERISTIC SHOWS HOW STRONGLY IT ENDED THE DAY. WHICH IS A FACTOR IN ITS OVER NIGHT SUCCESS.

GOOG$Range_pct_past = abs((GOOG$GOOG.Close - GOOG$GOOG.Open)/(GOOG$GOOG.High-GOOG$GOOG.Low)*100)
GOOG$Range_pct_atpr = (GOOG$Range/GOOG$GOOG.Close)*100
GOOG$william_range_pct = (GOOG$GOOG.High-GOOG$GOOG.Close)/(GOOG$GOOG.High-GOOG$GOOG.Low)*100

# 1 Month Range %
# Where is the stock on the last 20 days, 1 month range is the high of the last 20 days, 
# and the low is the low of the last 20 days, where did it close in that range.
#GOOG$One_month_Range_pct_past = SMA(GOOG$Range_pct_past, n = 20)
#GOOG$One_month_Range_pct_atpr = SMA(GOOG$Range_pct_atpr, n = 20 )
#GOOG$One_month_william_pct_range = SMA(GOOG$william_range_pct, n = 20 )


one_mth_range_pct <- rollapply(GOOG$GOOG.High, 20, max) - rollapply(GOOG$GOOG.Low, 20, max)

GOOG$one_month_range_pct = (GOOG$GOOG.Close- GOOG$GOOG.Low)/one_mth_range_pct*100

# EMA calculates an exponentially-weighted mean, giving more weight to recent observations. See Warning section below.
GOOG$EMA_10 <- EMA(GOOG$GOOG.Low, n = 10)

GOOG$EMA_20 <- EMA(GOOG$GOOG.Low, n = 20)

# WMA is similar to an EMA, but with linear weighting if the length of wts is equal to n. If the length of wts is equal to the length of x, the WMA will use the values of wts as weights.
GOOG$EMA_60 <- EMA(GOOG$GOOG.Low, n = 60)

# DEMA is calculated as: DEMA = (1 + v) * EMA(x,n) - EMA(EMA(x,n),n) * v (with the corresponding wilder and ratio arguments).
GOOG$DEMA_10 <- DEMA(GOOG$GOOG.Low, n = 10)

GOOG$WMA_10 <- WMA(GOOG$GOOG.Low, n = 10)

# EVWMA uses volume to define the period of the MA.
GOOG$EVWMA_10 <- EVWMA(GOOG$GOOG.Low, GOOG$GOOG.Volume)

# ZLEMA is similar to an EMA, as it gives more weight to recent observations, but attempts to remove lag by subtracting data prior to (n-1)/2 periods (default) to minimize the cumulative effect.
GOOG$ZLEMA_10 <- ZLEMA(GOOG$GOOG.Low, n = 10)

# VWMA and VWAP calculate the volume-weighted moving average price.
GOOG$VWAP_10 <- VWAP(GOOG$GOOG.Low, GOOG$GOOG.Volume)

# HMA a WMA of the difference of two other WMAs, making it very reponsive.
GOOG$HMA_10 <- HMA(GOOG$GOOG.Low, n = 20)

# ALMA inspired by Gaussian filters. Tends to put less weight on most recent observations, reducing tendency to overshoot.

GOOG$ALMA_10 <- ALMA(GOOG$GOOG.Low, n = 9, offset = 0.85, sigma = 6)




#GOOG <- GOOG[complete.cases(GOOG), ]
write.csv(GOOG, file = "GOOG_with_TI.csv", row.names = F)

#############################################
#              Model Development            #
#############################################

## Drop missing values
tail(GOOG)
dim(GOOG)


library(tseries, quietly = T)
adf.test(GOOG$GOOG.Adjusted)
# We will need to bring stationarity


library(forecast)
library(xts)
library(e1071)
library(doParallel)
library(dynlm)
library(caret)
library(dynlm)

GOOG_lm <- na.omit(GOOG)
dim(GOOG_lm)
set.seed(1)

head(GOOG_lm)
dim(GOOG_lm)
X <- GOOG_lm[,-6]
y <- GOOG_lm[,6]

# Scale the other variables
X.scaled <- scale(X)
# Merge back the dataframe
GOOG_lm <- cbind(X.scaled, y)

# Correlations with SalePrice
# Altogether, there are 10 numeric variables with a correlation of at least 0.5 with SalePrice. All those correlations are positive.

numericVars <- which(sapply(GOOG_lm, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables.\n')
## There are 37 numeric variables
all_numVar <- GOOG_lm[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'GOOG.Adjusted'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", number.cex=0.5)




# Remove highly correlated variables
tmp <- cor(GOOG_lm)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0
GOOG_lm <- GOOG_lm[,!apply(tmp,2,function(x) any(x > 0.90))]
head(GOOG_lm)

dim(X)
dim(GOOG_lm)


###############################
# Prepare Train and Test Data #
###############################

# Predict the next day, if you want to predict the more days values, change n
days_forecast = 7
n = days_forecast + 1
X_train = GOOG_lm[1:(nrow(GOOG_lm)-(n-1)),-18]
y_train = GOOG_lm[n:nrow(GOOG_lm),18]
X_test = GOOG_lm[((nrow(GOOG_lm)-(n-2)):nrow(GOOG_lm)),-18]
y_test = c()  
dim(X_train)
dim(X_test)
train <- cbind(X_train,y_train)

# Using "CARET"
# Hyper parameter Tuning
# Repeated Cross Validation
# Evaluation and Optimization

########################## 
# Lasso regression model #
##########################



# I have also tried other models, but since lasso gives the best results of those 3 models 
# I am only keeping the lasso model in the document.
# The elastic-net penalty is controlled by alpha, and bridges the gap between lasso (alpha=1) and ridge (alpha=0). 
# The tuning parameter lambda controls the overall strength of the penalty. It is known that the ridge penalty shrinks 
# the coefficients of correlated predictors towards each other while the lasso tends to pick one of them and discard the others.
# Below, I am using caret cross validation to find the best value for lambda, which is the only hyperparameter that needs to be tuned for the lasso model.

 train$GOOG.Adjusted <- as.numeric(train$GOOG.Adjusted)


# Set Random Seed
 set.seed(123)


 my_control <-trainControl(method="cv", number=5)
 lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

 lasso_mod <- train(GOOG.Adjusted~., data = na.omit(train), method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
 lasso_mod$bestTune

 min(lasso_mod$results$RMSE)

# The documentation of the caret `varImp' function says: for glmboost and glmnet the absolute value of the coefficients corresponding to the tuned model are used.
# 
# Although this means that a real ranking of the most important variables is not stored, it gives me the opportunity to find out how many of the variables are not used in the model (and hence have coefficient 0).


 lassoVarImp <- varImp(lasso_mod,scale=F)
 lassoImportance <- lassoVarImp$importance
 varsSelected <- length(which(lassoImportance$Overall!=0))
 varsNotSelected <- length(which(lassoImportance$Overall==0))
 cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.\n')

 LassoPred <- predict(lasso_mod, X_test)
# Prediction for next seven days
 LassoPred


 print("Next Seven Days Forecast using Lasso Regularised Regression will be : ")
 print(LassoPred)

#################
# XGBoost model #
#################


xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

# The next step is to let caret find the best hyperparameter values (using 5 fold cross validation).

library(xgboost)
xgb_caret <- train(GOOG.Adjusted~., data = na.omit(train), method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune

#     nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 24    1000         6 0.01     0                1                4         1

label_train <- y_train[!is.na(y_train)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))


# In addition, I am taking over the best tuned values from the caret cross validation.

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=4, #default=6
  min_child_weight=1, #default=1
  subsample=1,
  colsample_bytree=1
)


# The next step is to do cross validation to determine the best number of rounds (for the given set of parameters).
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F, verbose = TRUE)

# Although it was a bit of work, the hyperparameter tuning definitly paid of, as the cross validated RMSE inproved considerably (from 0.1225 without the caret tuning, to 0.1162 in this version)!

#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 500)
XGBpred <- predict(xgb_mod, dtest)
XGBpred

print("Next Seven Days Forecast using Extreme Boosting Regression will be : ")
print(XGBpred)


#########################
# Step 2: Build a model #
#########################

# Configure

#install.packages("devtools")
devtools::install_github("rstudio/keras")

#The above step will load the keras library from the GitHub repository. Now it is time to load keras into R and install tensorflow.
library(keras)
devtools::install_github("rstudio/tensorflow")
#By default RStudio loads the CPU version of tensorflow. Use the below command to download the CPU version of tensorflow.
library(tensorflow)
install_tensorflow()

#To install the tensorflow version with GPU support for a single user/desktop system, use the below command.
#install_tensorflow(gpu=TRUE)

k = ncol(X_train)

## create your model,and add layers 
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'linear')

summary(model)

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse',
  metrics = 'mse'
)

###########################
# Step 2: Train the model #
###########################

model %>% fit(X_train, y_train, epochs=100, batch_size=28, validation_split = 0.1)


################################
# Step 3: Plot the predictions #
################################

pred <- model %>% predict(X_test, batch_size = 28)
print("Next Seven Days Forecast using Neural Network Regression will be : ")
print(pred)


