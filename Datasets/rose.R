# Install and load necessary packages
# install.packages("ROSE")

library(ROSE)

# Load the original dataset
data <- read.csv("Dataset.csv")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Identify numeric columns for SMOTE
numeric_cols <- sapply(data, is.numeric)

# Separate the dataset into features and target variable
features <- data[, numeric_cols, drop = FALSE]
target <- data$Attrition

# Apply SMOTE using ROSE
rose_data <- ROSE(target ~ ., data = cbind(target, features), seed = 123, N = 3000)$data

# Save the new dataset as smote.csv
write.csv(rose_data, "smote.csv", row.names = FALSE)

# Print a message indicating successful completion
cat("SMOTE applied successfully using ROSE. New dataset saved as smote.csv.\n")
