# Load necessary library
library(ggplot2)

# Read the data
data <- read.csv("information_gain_scores.csv")

# Create a horizontal bar plot
plot <- ggplot(data, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Information Gain for Features",
       x = "Information Gain",
       y = "Feature") +
  theme_minimal()

# Print the plot
print(plot)
