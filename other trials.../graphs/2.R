# Load necessary libraries
library(ggplot2)
library(tidyr)

# Random oversampled data
random_data <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"),
  Accuracy = c(0.723893805, 0.962831858, 0.92920354, 0.784070796, 0.746902655, 0.961061947),
  Sensitivity = c(0.739776952, 0.945945946, 0.888888889, 0.780068729, 0.841346154, 0.996226415),
  Precision = c(0.698245614, 0.98245614, 0.98245614, 0.796491228, 0.614035088, 0.926315789),
  Specificity = c(0.709459459, 0.981412639, 0.98, 0.788321168, 0.691876751, 0.93)
)

# Both over and under sampled data
both_data <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"),
  Accuracy = c(0.782978723, 0.919148936, 0.842553191, 0.859574468, 0.778723404, 0.914893617),
  Sensitivity = c(0.76744186, 0.897637795, 0.807407407, 0.859504132, 0.846938776, 0.916666667),
  Precision = c(0.825, 0.95, 0.908333333, 0.866666667, 0.691666667, 0.916666667),
  Specificity = c(0.801886792, 0.944444444, 0.89, 0.859649123, 0.729927007, 0.913043478)
)

# Reshape the data for plotting
random_data_long <- gather(random_data, key = "Metric", value = "Value", -Algorithm)
both_data_long <- gather(both_data, key = "Metric", value = "Value", -Algorithm)

# Add a column to identify the dataset
random_data_long$Dataset <- "Random Oversampled"
both_data_long$Dataset <- "Both Over and Under Sampled"

# Combine both datasets
combined_data <- rbind(random_data_long, both_data_long)

# Create the plot
plot <- ggplot(combined_data, aes(x = Algorithm, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Dataset, scales = "free_y") +
  labs(title = "Comparison of Performance Metrics",
       x = "Algorithm",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)
