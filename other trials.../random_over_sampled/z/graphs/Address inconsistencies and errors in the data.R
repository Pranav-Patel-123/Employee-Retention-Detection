# Load the dataset
data <- read.csv("random_over_sampled.csv")

# 1. Identify Inconsistencies
summary(data)
str(data)

# 2. Handle Missing Values
# Example: Impute missing values with the median for numerical variables
numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# 3. Resolve Duplicate Records
data <- unique(data)

# 4. Correct Data Types
# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# 5. Detect and Remove Outliers
# Implement outlier detection and removal using Z-score method
remove_outliers <- function(x) {
  threshold <- 3
  is_outlier <- abs(scale(x)) > threshold
  x[is_outlier] <- NA
  return(x)
}

numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], remove_outliers)

# 6. Validate Logical Constraints
# Example: Check if numerical values fall within valid ranges
validate_ranges <- function(x) {
  valid_range <- c(0, 100)  # Example: Valid range for numerical values
  return(ifelse(x < valid_range[1] | x > valid_range[2], NA, x))
}

numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], validate_ranges)

# 7. Transform Data
# Example: Normalize numerical columns
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], normalize)

# 8. Validate Data Consistency
# Example: Check for consistency between related variables
validate_consistency <- function(data) {
  if (any(complete.cases(data$MonthlyIncome, data$DailyRate)) && any(data$MonthlyIncome < data$DailyRate)) {
    warning("MonthlyIncome should be greater than or equal to DailyRate.")
  }
  # Add more consistency checks as needed
}

validate_consistency(data)

# Output cleaned dataset
write.csv(data, "cleaned_data.csv", row.names = FALSE)