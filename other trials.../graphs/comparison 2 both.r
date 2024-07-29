# Load necessary library
library(ggplot2)

# Create data frame with accuracies for each algorithm and feature selection technique
data <- data.frame(
  Algorithm = rep(c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"), 8),
  Feature_Selection = rep(c("All Features", "CFS", "Chi Square", "Information Gain", "LASSO", "Genetic Algorithm", "Importance Score", "RFE"), each = 6),
  Accuracy = c(
    0.782978723, 0.919148936, 0.842553191, 0.859574468, 0.778723404, 0.914893617,
    0.727659574, 0.880851064, 0.855319149, 0.723404255, 0.761702128, 0.914893617,
    0.770212766, 0.89787234, 0.885106383, 0.710638298, 0.689361702, 0.855319149,
    0.782978723, 0.89787234, 0.85106383, 0.817021277, 0.719148936, 0.914893617,
    0.561702128, 0.846808511, 0.812765957, 0.604255319, 0.608510638, 0.672340426,
    0.604255319, 0.787234043, 0.795744681, 0.582978723, 0.6, 0.646808511,
    0.753191489, 0.880851064, 0.859574468, 0.710638298, 0.680851064, 0.893617021,
    0.782978723, 0.919148936, 0.872340426, 0.812765957, 0.765957447, 0.89787234
  )
)

# Define line types and colors
line_types <- c("solid", "dotted", "dashed", "dotdash", "longdash", "twodash", "dotdash", "dotted")
colors <- c("red", "green", "blue", "orange", "purple", "pink", "gray", "cyan")

# Create the plot
plot <- ggplot(data, aes(x = Algorithm, y = Accuracy, group = Feature_Selection, color = Feature_Selection, linetype = Feature_Selection)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_linetype_manual(values = line_types) +
  scale_color_manual(values = colors) +
  labs(title = "Accuracy of Algorithms with Different Feature Selection Techniques",
       x = "Algorithm",
       y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)
