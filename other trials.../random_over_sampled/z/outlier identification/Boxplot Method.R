# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using boxplot method
outliers <- boxplot(data[, sapply(data, is.numeric)], plot = FALSE)$out

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Boxplot method in the following rows:\n")
  print(data[data[, sapply(data, is.numeric)] %in% outliers, ])
  
  # Remove outliers
  data_cleaned <- data[!data[, sapply(data, is.numeric)] %in% outliers, ]
  
  cat("\nOutliers removed using Boxplot method.\n")
} else {
  cat("No outliers detected using Boxplot method.\n")
}

# Save the cleaned dataset
write.csv(data_cleaned, "boxplot_cleaned.csv", row.names = FALSE)
