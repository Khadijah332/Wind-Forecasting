#Load the training data 
train_data <- read.csv("train.csv")

#View the first rows
head(train_data)

#View the structure of the dataset
str(train_data)

# Plot the power generation for wind farm 1 (wp1) for the first 100 hours
plot(train_data$wp1[1:100], type = "l", col = "blue", 
     main = "Wind Farm 1 Power (First 100 Hours)", 
     xlab = "Time Index", ylab = "Power Output")