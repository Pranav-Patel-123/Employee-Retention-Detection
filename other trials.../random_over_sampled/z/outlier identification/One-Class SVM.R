# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Identify outliers using One-Class SVM
library(e1071)
svm_model <- svm(data[, sapply(data, is.numeric)], type = "one-classification", nu = 0.1)
outliers <- which(predict(svm_model, data[, sapply(data, is.numeric)]) == -1)

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using One-Class SVM method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using One-Class SVM method.\n")
} else {
  cat("No outliers detected using One-Class SVM method.\n")
}
# Save the cleaned dataset
#write.csv(data_cleaned, "one_class_svm_cleaned.csv", row.names = FALSE)
# Apply manual scaling for normalization
scaled_data <- as.data.frame(lapply(data_cleaned, function(x) (x - min(x)) / (max(x) - min(x))))

# Overwrite the 'data' variable with the normalized data
data <- scaled_data

