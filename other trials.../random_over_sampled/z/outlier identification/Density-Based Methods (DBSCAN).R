# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using density-based method (DBSCAN)
library(dbscan)

# Set the value for the eps parameter
eps_value <- 0.5  # You may need to adjust this value based on your data

outliers <- which(dbscan(data[, sapply(data, is.numeric)], eps = eps_value)$cluster == 0)

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Density-Based method (DBSCAN) in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using Density-Based method (DBSCAN).\n")
} else {
  cat("No outliers detected using Density-Based method (DBSCAN).\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "density_based_cleaned.csv", row.names = FALSE)
