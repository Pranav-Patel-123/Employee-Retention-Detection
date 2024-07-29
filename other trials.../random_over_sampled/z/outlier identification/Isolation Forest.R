# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using Local Outlier Factor (LOF)
library(dbscan)
outliers <- which(lof(data[, sapply(data, is.numeric)]) > 1.5)

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Local Outlier Factor (LOF) method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using Local Outlier Factor (LOF) method.\n")
} else {
  cat("No outliers detected using Local Outlier Factor (LOF) method.\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "lof_cleaned.csv", row.names = FALSE)
