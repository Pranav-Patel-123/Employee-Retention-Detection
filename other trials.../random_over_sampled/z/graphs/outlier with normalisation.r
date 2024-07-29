# Increase max.print option to show more rows
options(max.print = 1000)  # You can adjust this number based on your preference

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)

# Remove duplicates and convert to a vector
outliers <- unique(unlist(outliers))

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Z-score method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using Z-score method.\n")
} else {
  cat("No outliers detected using Z-score method.\n")
}

# Apply manual scaling for normalization
scaled_data <- as.data.frame(lapply(data_cleaned, function(x) (x - min(x)) / (max(x) - min(x))))

# Overwrite the 'data' variable with the normalized data
data <- scaled_data

# Save the cleaned and normalized dataset
#write.csv(data, "cleaned_and_normalized.csv", row.names = FALSE)
