# Load necessary library
library(ggplot2)
library(tidyr)

# Create data frame with performance metrics for each algorithm
data <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"),
  Accuracy = c(0.782978723, 0.919148936, 0.872340426, 0.812765957, 0.765957447, 0.89787234),
  Sensitivity = c(0.76744186, 0.910569106, 0.8515625, 0.81147541, 0.828282828, 0.887096774),
  Precision = c(0.825, 0.933333333, 0.908333333, 0.825, 0.683333333, 0.916666667),
  Specificity = c(0.801886792, 0.928571429, 0.897196262, 0.814159292, 0.720588235, 0.90990991)
)

# Reshape the data for plotting
data_long <- gather(data, key = "Metric", value = "Value", -Algorithm)

# Create the plot
plot <- ggplot(data_long, aes(x = Metric, y = Value, group = Algorithm, color = Algorithm)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Performance Metrics of Algorithms",
       x = "Performance Metric",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)
