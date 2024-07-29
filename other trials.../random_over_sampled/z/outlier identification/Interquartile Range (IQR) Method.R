# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using IQR method
numeric_columns <- data[, sapply(data, is.numeric)]
outliers <- sapply(numeric_columns, function(x) {
  q <- quantile(x, c(0.25, 0.75))
  lower <- q[1] - 1.5 * IQR(x)
  upper <- q[2] + 1.5 * IQR(x)
  x < lower | x > upper
})

# Get indices of outliers
outlier_indices <- which(outliers, arr.ind = TRUE)

# Remove outliers if found
if (length(outlier_indices) > 0) {
  cat("Outliers detected using IQR method in the following rows:\n")
  print(data[outlier_indices, ])
  
  # Remove outliers
  data_cleaned <- data[-outlier_indices, ]
  
  cat("\nOutliers removed using IQR method.\n")
} else {
  cat("No outliers detected using IQR method.\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "iqr_cleaned.csv", row.names = FALSE)
