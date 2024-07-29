# Load the required libraries
library(caTools)
library(caret)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Define control parameters for recursive feature elimination
train_control <- trainControl(method = "cv", number = 10)

# Perform Recursive Feature Elimination with Cross-Validation (RFECV)
model <- rfe(data[, -which(names(data) == "Attrition")], data$Attrition, sizes = c(1:ncol(data)-1), rfeControl = train_control)

# Print selected features
cat("Selected features based on RFECV:\n", names(model$optVariables), "\n")
