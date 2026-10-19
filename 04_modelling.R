# Script Name: 04_modelling.R
# Purpose: Feature engineering and baseline model

#load random forest library
library(randomForest)

#load data
# Load data
train_data <- read.csv("processed_data/train_cleaned.csv")
train_data$datetime <- as.POSIXct(train_data$datetime, tz = "UTC")

#feature engineering - lag features for wind farm 1 (wp1) - 1 hour and 24hour
train_data$wp1_lag1  <- c(NA, head(train_data$wp1, -1))       # 1 hour ago
train_data$wp1_lag24 <- c(rep(NA, 24), head(train_data$wp1, -24)) # 24 hours ago

#remove rows with NA values created by the lags
model_data <- na.omit(train_data)

#train/test split - 80/20
split_index <- floor(nrow(model_data) * 0.8)
train_set <- model_data[1:split_index, ]
test_set  <- model_data[(split_index + 1):nrow(model_data), ]

#build baseline model - persistance model
#tommorows wind will be exactly what it was 24hours ago
test_set$persistence_pred <- test_set$wp1_lag1

#evaluate using Mean Absolute Error (MAE)
mae_persistence <- mean(abs(test_set$wp1 - test_set$persistence_pred))

cat("--- Baseline Persistence Model Results ---\n")
cat("Test MAE:", round(mae_persistence, 4), "\n")

#train a random forest model using lag features and temporal features to predict wind power
set.seed(42) #to make results reproducible 
rf_model <- randomForest(
  wp1 ~ wp1_lag1 + wp1_lag24 + hour + month,
  data = train_set,
  ntree = 100,      #number of decision trees to grow
  importance = TRUE #tracks which features are most useful
)

#predict on the Test Set
test_set$rf_pred <- predict(rf_model, newdata = test_set)

#evaluate Random Forest MAE
mae_rf <- mean(abs(test_set$wp1 - test_set$rf_pred))

cat("--- Random Forest Model Results ---\n")
cat("Test MAE:", round(mae_rf, 4), "\n")

#feature Importance Check
print(importance(rf_model))