# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Visual inspection of data distribution
numeric_data <- data[, sapply(data, is.numeric)]
par(mfrow = c(3, 3))
for (i in 1:ncol(numeric_data)) {
  hist(numeric_data[, i], main = colnames(numeric_data)[i])
}

# Identify and remove outliers incrementally
for (threshold in c(3, 3.5, 4, 4.5, 5)) {
  outliers <- sapply(data[, numeric_columns], function(x) which(abs(scale(x)) > threshold, arr.ind = TRUE))
  outliers <- unique(unlist(outliers))
  
  if (length(outliers) == 0) {
    cat("No outliers detected using threshold:", threshold, "\n")
  } else {
    cat("Outliers detected using threshold:", threshold, "\n")
    print(data[outliers, ])
    
    # Remove outliers
    data <- data[-outliers, ]
    
    cat("Outliers removed from the dataset.\n")
    
    # Exit loop if outliers are removed
    break
  }
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
