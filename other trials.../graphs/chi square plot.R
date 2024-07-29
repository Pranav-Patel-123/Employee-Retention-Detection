# Load necessary library
library(ggplot2)

# Read the data
data <- read.csv("chi_square_scores.csv")

# Create a horizontal bar plot
plot <- ggplot(data, aes(x = ChiSquareScore, y = reorder(Feature, ChiSquareScore))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Chi-Square Scores for Features",
       x = "Chi-Square Score",
       y = "Feature") +
  theme_minimal()

# Print the plot
print(plot)
