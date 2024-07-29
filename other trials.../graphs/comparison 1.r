# Load necessary library
library(ggplot2)

# Create data frame with accuracies for each algorithm and feature selection technique
data <- data.frame(
  Algorithm = rep(c("Decision Tree", "Random Forest", "KNN", "Logistic Regression", "Naïve Bayes", "SVM"), 8),
  Feature_Selection = rep(c("All Features", "CFS", "Chi Square", "Information Gain", "LASSO", "Genetic Algorithm", "Importance Score", "RFE"), each = 6),
  Accuracy = c(
    0.723893805, 0.962831858, 0.92920354, 0.784070796, 0.746902655, 0.961061947,
    0.716814159, 0.938053097, 0.925663717, 0.709734513, 0.739823009, 0.961061947,
    0.720353982, 0.94159292, 0.918584071, 0.713274336, 0.690265487, 0.884955752,
    0.709734513, 0.946902655, 0.922123894, 0.771681416, 0.730973451, 0.962831858,
    0.642477876, 0.950442478, 0.897345133, 0.654867257, 0.642477876, 0.787610619,
    0.603539823, 0.911504425, 0.884955752, 0.546902655, 0.571681416, 0.607079646,
    0.70619469, 0.950442478, 0.907964602, 0.716814159, 0.677876106, 0.918584071,
    0.713274336, 0.962831858, 0.925663717, 0.789380531, 0.750442478, 0.959292035
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
