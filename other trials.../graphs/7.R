# Load necessary libraries
library(ggplot2)
library(tidyr)


# Random oversampled data
random_data <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"),
  Accuracy = c(0.713274336, 0.962831858, 0.925663717, 0.789380531, 0.750442478, 0.959292035),
  Sensitivity = c(0.759493671, 0.945945946, 0.883280757, 0.788194444, 0.830275229, 0.992481203),
  Precision = c(0.631578947, 0.98245614, 0.98245614, 0.796491228, 0.635087719, 0.926315789),
  Specificity = c(0.679878049, 0.981412639, 0.97983871, 0.790613718, 0.700288184, 0.929765886)
)

# Both over and under sampled data
both_data <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"),
  Accuracy = c(0.782978723, 0.919148936, 0.872340426, 0.812765957, 0.765957447, 0.89787234),
  Sensitivity = c(0.76744186, 0.910569106, 0.8515625, 0.81147541, 0.828282828, 0.887096774),
  Precision = c(0.825, 0.933333333, 0.908333333, 0.825, 0.683333333, 0.916666667),
  Specificity = c(0.801886792, 0.928571429, 0.897196262, 0.814159292, 0.720588235, 0.90990991)
)

# Reshape the data for plotting
random_data_long <- gather(random_data, key = "Metric", value = "Value", -Algorithm)
both_data_long <- gather(both_data, key = "Metric", value = "Value", -Algorithm)

# Add a column to identify the dataset
random_data_long$Dataset <- "Random Oversampled"
both_data_long$Dataset <- "Both Over and Under Sampled"

# Combine both datasets
combined_data <- rbind(random_data_long, both_data_long)

# Plot the two-sided bar plot with aligned bars
plot <- ggplot(combined_data, aes(x = Value, y = Algorithm, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +  # Adjust position and width
  facet_grid(. ~ Dataset, scales = "free_x", space = "free_x") +
  labs(title = "Comparison of above two matrices with RFE selected features: ",
       x = "Value",
       y = "Algorithm",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=8))

# Print the plot
print(plot)


