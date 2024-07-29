# Load necessary library
library(ggplot2)

# Read the data
data <- read.csv("genetic_algorithm.csv", header = FALSE)

# Rename columns
colnames(data) <- c("Feature", "Score")

# Filter out missing or non-numeric values
data <- na.omit(data[data$Score != "NA" & !is.na(as.numeric(data$Score)), ])

# Convert Score column to numeric
data$Score <- as.numeric(data$Score)

# Create a horizontal bar plot with features on the y-axis
plot <- ggplot(data, aes(x = reorder(Feature, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Genetic Algorithm Scores for Features",
       x = "Feature",
       y = "Score") +
  theme_minimal() +
  coord_flip() +  # Flips the plot horizontally
  scale_y_continuous(breaks = seq(min(data$Score), max(data$Score), length.out = 6))  # Specify the number of breaks

# Print the plot
print(plot)
