# Load necessary library
library(ggplot2)

# Read the data
data <- read.csv("importance_scores.csv", header = TRUE)

# Check the structure of the data to ensure it's read correctly
str(data)

# Ensure the column names are correctly spelled and capitalized
colnames(data) <- c("Feature", "MeanDecreaseGini")  # Adjust column names as needed

# Create a horizontal bar plot
plot <- ggplot(data, aes(x = MeanDecreaseGini, y = reorder(Feature, MeanDecreaseGini))) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Feature Importance (Mean Decrease Gini)",
       x = "Mean Decrease Gini",
       y = "Feature") +
  theme_minimal()

# Print the plot
print(plot)
