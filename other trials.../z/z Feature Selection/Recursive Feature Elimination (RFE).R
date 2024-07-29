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

# Apply RFE feature selection
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
selected_features <- rfe(data[, -which(names(data) == "Attrition")], data$Attrition, sizes = c(1:ncol(data)-1), rfeControl = ctrl)

# Print selected features
cat("Selected features based on RFE:\n", selected_features$optVariables, "\n")
