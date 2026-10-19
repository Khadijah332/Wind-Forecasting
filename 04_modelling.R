# Script Name: 04_modelling.R
# Purpose: Feature engineering and baseline model

library(zoo)
library(xgboost)

#load cleaned data
train_data <- read.csv("processed_data/train_cleaned.csv")
train_data$datetime <- as.POSIXct(train_data$datetime, tz = "UTC")

#lag features
train_data$wp1_lag1  <- c(NA, head(train_data$wp1, -1))
train_data$wp1_lag24 <- c(rep(NA, 24), head(train_data$wp1, -24))

#rolling features to capture trend over time
#3 hour average 
train_data$wp1_roll_mean3 <- rollmean(train_data$wp1, k = 3, fill = NA, align = "right")
#24 hour moving average
train_data$wp1_roll_mean24 <- rollmean(train_data$wp1, k = 24, fill = NA, align = "right")
#6 hour rolling volatility
#looks foward 6 hours to measure the values to assess the unstabillity of the wind using standard deivation to check that
train_data$wp1_roll_sd6 <- rollapply(train_data$wp1, width = 6, FUN = sd, fill = NA, align = "right")


#remove rows with NA Values
model_data <- na.omit(train_data)

#train/test - 80/20 - chronological to split the time
split_index <- floor(nrow(model_data) * 0.8)
train_set <- model_data[1:split_index, ]
test_set  <- model_data[(split_index + 1):nrow(model_data), ]

#baseline presistance model (benchmark)
test_set$persistence_pred <- test_set$wp1_lag1
mae_persistence <- mean(abs(test_set$wp1 - test_set$persistence_pred))

#multiple linear regression with rolling features
lin_model <- lm(wp1 ~ wp1_lag1 + wp1_lag24 + wp1_roll_mean3 + wp1_roll_mean24 + wp1_roll_sd6 + hour + month, 
                data = train_set)
test_set$lin_pred <- predict(lin_model, newdata = test_set)

#setting boundarys so that it stays between 0 and 1 
test_set$lin_pred <- pmax(0, pmin(1, test_set$lin_pred))
mae_lin <- mean(abs(test_set$wp1 - test_set$lin_pred))

#XGBOOST
#prepare feature matrices - it uses matrices not dataframes
feature_cols <- c("wp1_lag1", "wp1_lag24", "wp1_roll_mean3", "wp1_roll_mean24", "wp1_roll_sd6", "hour", "month")

train_matrix <- xgb.DMatrix(data = as.matrix(train_set[, feature_cols]), label = train_set$wp1)
test_matrix  <- xgb.DMatrix(data = as.matrix(test_set[, feature_cols]), label = test_set$wp1)

#set hyperparamaters for time-series boosting
params <- list(
  objective = "reg:squarederror",
  eta = 0.05,             #learning rate (slow and steady)
  max_depth = 4,          #shallow trees to prevent overfitting
  subsample = 0.8,        #row sampling
  colsample_bytree = 0.8  #feature sampling
)

#train the XGBoost model
set.seed(42)
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 200,          #number of boosting rounds/trees
  verbose = 0
)

#predict and set bounds
test_set$xgb_pred <- predict(xgb_model, test_matrix)
test_set$xgb_pred <- pmax(0, pmin(1, test_set$xgb_pred))

#evaluate XGBoost MAE
mae_xgb <- mean(abs(test_set$wp1 - test_set$xgb_pred))

# print results to compare
cat("       Wind forecasting results     \n")
cat("Baseline Persistence MAE :", round(mae_persistence, 5), "\n")
cat("Advanced Linear Model MAE:", round(mae_lin, 5), "\n")
cat("XGBoost Boosting MAE     :", round(mae_xgb, 5), "\n")


