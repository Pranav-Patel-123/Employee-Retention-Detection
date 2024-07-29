# Load the required libraries
library(caTools)
library(FSelector)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Apply Recursive Feature Addition (RFA)
selected_features <- rfa(Attrition ~ ., data = data)

# Print selected features
cat("Selected features based on Recursive Feature Addition (RFA):\n", selected_features, "\n")
