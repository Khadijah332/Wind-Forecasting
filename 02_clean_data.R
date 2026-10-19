# Script Name: 02_clean_data.R
# Purpose: transform dates and extract time features for wind forecasting

#load the raw data
train_data <- read.csv("train.csv")

# convert integer data into POSIXct datetime object
train_data$datetime <- as.POSIXct(
  as.character(train_data$date), #change to text 
  format = "%Y%m%d%H", #pass into this pattern
  tz = "UTC" #timezone
)

# feature engineering to extract hour and month
train_data$hour <- as.numeric(format(train_data$datetime, "%H"))
train_data$month <- as.numeric(format(train_data$datetime, "%m"))

# sanity check
# old date vs new columns
head(train_data[, c("date", "datetime", "hour", "month")])

# check structure to view new columns
str(train_data)


