# Script Name: 01_load_data.R
# Purpose: Load raw wind forecasting data and preform initial sanity checks

#Load necessary libraries
library(dplyr)

#load the raw datasets
train_data <- read.csv("train.csv")
test_data  <- read.csv("test.csv")
benchmark  <- read.csv("benchmark.csv")

#sanity check and verifications
cat("---Train Data Summary---\n")
#check the size of the dataset - number of rows & columns
print(dim(train_data)) 
#check the structure of the dataset
str(train_data)

cat("---Check for Missing Values---\n")
print(colSums(is.na(train_data)))

#show the first few rows
cat("---First Few Rows---\n")
head(train_data)