# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Install and load the caret package
#install.packages("caret")
library(caret)

# Identify outliers using distance-based method (k-nearest neighbors)
outliers <- nearZeroVar(data[, sapply(data, is.numeric)], saveMetrics = TRUE)$nzv

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Distance-Based method (k-nearest neighbors) in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using Distance-Based method (k-nearest neighbors).\n")
} else {
  cat("No outliers detected using Distance-Based method (k-nearest neighbors).\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "distance_based_cleaned.csv", row.names = FALSE)
