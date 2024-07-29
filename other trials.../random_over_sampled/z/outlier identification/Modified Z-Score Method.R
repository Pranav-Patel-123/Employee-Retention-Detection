# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using modified Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)], center = apply(data[, sapply(data, is.numeric)], 2, median), scale = apply(data[, sapply(data, is.numeric)], 2, mad))) > 3, arr.ind = TRUE)

# Remove duplicates and convert to a vector
outliers <- unique(unlist(outliers))

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Modified Z-score method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using Modified Z-score method.\n")
} else {
  cat("No outliers detected using Modified Z-score method.\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "modified_zscore_cleaned.csv", row.names = FALSE)
