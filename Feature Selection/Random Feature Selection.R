# Load the required libraries
library(caTools)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Perform random feature selection
#set.seed(123) # Set seed for reproducibility
selected_indices <- sample(2:ncol(data), size = 5, replace = FALSE) # Select 5 random features (excluding target variable)
selected_features <- names(data)[selected_indices]

# Print selected features
cat("Selected features based on Random Feature Selection:\n", selected_features, "\n")
