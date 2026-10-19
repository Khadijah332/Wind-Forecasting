# Script Name: 04_modelling.R
# Purpose: Train/Validation/Test split, hyperparmeter tuning and model selection

library(zoo)
library(randomForest)
library(xgboost)

#load clean data
train_data <- read.csv("processed_data/train_cleaned.csv")
train_data$datetime <- as.POSIXct(train_data$datetime, tz = "UTC")

#feature engineering
# Look back 1 hour
train_data$wp1_lag1       <- c(NA, head(train_data$wp1, -1))
# Look back 24 hours (1 day)
train_data$wp1_lag24      <- c(rep(NA, 24), head(train_data$wp1, -24))
# 3-hour average for short-term trend
train_data$wp1_roll_mean3 <- rollmean(train_data$wp1, k = 3, fill = NA, align = "right")
# 24-hour average for long-term trend
train_data$wp1_roll_mean24<- rollmean(train_data$wp1, k = 24, fill = NA, align = "right")
# 6-hour volatility (measures wind turbulence)
train_data$wp1_roll_sd6   <- rollapply(train_data$wp1, width = 6, FUN = sd, fill = NA, align = "right")
# Remove empty rows with missing values
model_data <- na.omit(train_data)


#60% train, 20% validation, 20% test)
n <- nrow(model_data)
train_end <- floor(n * 0.60)
val_end   <- floor(n * 0.80)

train_set <- model_data[1:train_end, ]
val_set   <- model_data[(train_end + 1):val_end, ]
test_set  <- model_data[(val_end + 1):n, ]

feature_cols <- c("wp1_lag1", "wp1_lag24", "wp1_roll_mean3", "wp1_roll_mean24", "wp1_roll_sd6", "hour", "month")

#train models

#baseline persistance between 0 and 1
test_set$persistence_pred <- pmax(0, pmin(1, test_set$wp1_lag1))

#RANDOM FOREST
# Set random seed for reproducibility
set.seed(42)

# Train a Random Forest model using features and time variables
rf_model <- randomForest(wp1 ~ wp1_lag1 + wp1_lag24 + wp1_roll_mean3 + wp1_roll_mean24 + wp1_roll_sd6 + hour + month,
                         data = train_set, ntree = 100)

# Predict with Random Forest between 0 and 1
test_set$rf_pred <- pmax(0, pmin(1, predict(rf_model, newdata = test_set)))

#LINEAR REGRESSION
lin_model <- lm(wp1 ~ wp1_lag1 + wp1_lag24 + wp1_roll_mean3 + wp1_roll_mean24 + wp1_roll_sd6 + hour + month, 
                data = train_set)

# Predict with Linear Regression and clip bounds between 0 and 1
test_set$lin_pred <- pmax(0, pmin(1, predict(lin_model, newdata = test_set)))

#XGBOOST
# Convert training set into an XGBoost matrix
dtrain <- xgb.DMatrix(data = as.matrix(train_set[, feature_cols]), label = train_set$wp1)

# Convert validation set into an XGBoost matrix
dval   <- xgb.DMatrix(data = as.matrix(val_set[, feature_cols]), label = val_set$wp1)

# Convert test set into an XGBoost matrix
dtest  <- xgb.DMatrix(data = as.matrix(test_set[, feature_cols]), label = test_set$wp1)

# Define hyperparameters for gradient boosting
params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 4, subsample = 0.8, colsample_bytree = 0.8)

# Set up watchlist to track training and validation errors
watchlist <- list(train = dtrain, val = dval)

# Set random seed for reproducibility
set.seed(42)

# Train XGBoost with early stopping using validation data
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  watchlist = watchlist,
  early_stopping_rounds = 20, 
  verbose = 0
)

# Predict with XGBoost  between 0 and 1
test_set$xgb_pred <- pmax(0, pmin(1, predict(xgb_model, dtest)))


#EVALUATE AND COMPARE
# Calculate MAE for persistence baseline
mae_persistence <- mean(abs(test_set$wp1 - test_set$persistence_pred))

# Calculate MAE for Random Forest
mae_rf          <- mean(abs(test_set$wp1 - test_set$rf_pred))
 
# Calculate MAE for Linear Regression
mae_lin         <- mean(abs(test_set$wp1 - test_set$lin_pred))

# Calculate MAE for XGBoost
mae_xgb         <- mean(abs(test_set$wp1 - test_set$xgb_pred))

# Combine all model scores into a comparison table
results_df <- data.frame(
  Model = c("Persistence Baseline", "Random Forest", "Linear Regression", "XGBoost"),
  Test_MAE = c(mae_persistence, mae_rf, mae_lin, mae_xgb)
)

# Sort results to find the best model automatically
results_df <- results_df[order(results_df$Test_MAE), ]

#Print table
cat("FINAL MODEL COMPARISON TABLE \n")
# Print the sorted results table
print(results_df)
