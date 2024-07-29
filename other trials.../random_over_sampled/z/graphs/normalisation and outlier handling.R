# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Print data types of all columns
print("Data types of all columns:")
print(sapply(data, class))

# Identify and remove outliers
numeric_columns <- sapply(data, is.numeric)
outliers <- sapply(data[, numeric_columns], function(x) which(abs(scale(x)) > 4, arr.ind = TRUE))
outliers <- unique(unlist(outliers))

if (length(outliers) > 0) {
  cat("Outliers detected in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data <- data[-outliers, ]
  
  cat("\nOutliers removed from the dataset.\n")
} else {
  cat("No outliers detected in the dataset.\n")
}

# Manual scaling normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to numerical columns
numeric_columns <- sapply(data, is.numeric)
if (sum(numeric_columns) > 0) {
  data[, numeric_columns] <- lapply(data[, numeric_columns], normalize)
  
  cat("\nDataset normalized.\n")
} else {
  cat("\nNo numerical columns found in the dataset.\n")
}

# Output new dataset
print("\nNew dataset:")
print(head(data))
