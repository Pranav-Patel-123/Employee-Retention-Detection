# Function to calculate chi-square statistic
chi_square <- function(feature, target) {
  observed <- table(feature, target)
  expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
  chi_square <- sum((observed - expected)^2 / expected)
  return(chi_square)
}

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Calculate chi-square statistic for each feature
chi_square_scores <- sapply(data[, -which(names(data) == "Attrition")], 
                            function(x) chi_square(x, data$Attrition))

# Sort features based on chi-square scores
sorted_features <- names(sort(chi_square_scores, decreasing = TRUE))

# Select top features (you can change the number of features)
selected_features <- sorted_features[1:10]

# Print selected features based on chi-square scores
cat("Selected features based on Chi-square Test:\n", selected_features, "\n")
