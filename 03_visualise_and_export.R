# Script Name: 01_load_data.R
# Purpose: Create multi-farm visualiisation & powerBI prep

library(ggplot2)
library(reshape2)

#load the raw data and apply cleaning steps
train_data <- read.csv("train.csv")

train_data$datetime <- as.POSIXct(
  as.character(train_data$date), 
  format = "%Y%m%d%H", 
  tz = "UTC"
)
train_data$hour <- as.numeric(format(train_data$datetime, "%H"))
train_data$month <- as.numeric(format(train_data$datetime, "%m"))

#reshape data for ggplot to put multiple wind farms into a single columns
subset_data <- train_data[1:300, c("datetime", "wp1", "wp2", "wp3", "wp4")]
melted_wind <- melt(subset_data, id.vars = "datetime")

#create a ggplot
cool_plot <- ggplot(melted_wind, aes(x = datetime, y = value, color = variable)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") + # Splits each farm into its own mini-panel!
  theme_minimal(base_size = 12) +
  labs(
    title = "Wind Power Generation Volatility Across Multiple Farms",
    subtitle = "GEFCom 2012 Training Data | First 300 Hours",
    x = "Timeline (UTC)",
    y = "Normalized Power Output",
    color = "Wind Farm"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none" # Since facet labels show the farm name
  )

# Print the plot to your RStudio viewer
print(cool_plot)

# 4. Export for Power BI
if (!dir.exists("processed_data")) { dir.create("processed_data") }
write.csv(train_data, "processed_data/train_cleaned.csv", row.names = FALSE)


