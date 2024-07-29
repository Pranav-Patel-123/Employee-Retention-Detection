# Load the required libraries
library(caTools)
library(stabs)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Perform Stability Selection
stability_result <- stabs(data[, -which(names(data) == "Attrition")], data$Attrition, method = "rf", B = 50)

# Print selected features
selected_features <- names(stability_result$selected)
cat("Selected features based on Stability Selection:\n", selected_features, "\n")
