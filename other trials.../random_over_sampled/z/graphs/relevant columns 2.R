# Load necessary packages
library(randomForest)  # For random forest algorithm
library(caret)          # For data preprocessing

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Shuffle the data (optional, as it's already shuffled in the original code)
set.seed(123)
data <- data[sample(nrow(data)), ]

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Attrition, p = 0.75, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Define predictors and target variable
predictors <- setdiff(names(data), "Attrition")  # Exclude target variable
target <- "Attrition"

# Train random forest model
rf_model <- randomForest(x = train_data[, predictors],
                         y = train_data[, target],
                         ntree = 500,  # Number of trees in the forest
                         importance = TRUE,  # Calculate variable importance
                         proximity = FALSE)  # Do not calculate proximity matrix

# Feature importance
importance <- importance(rf_model)

# Calculate total importance score by summing MeanDecreaseAccuracy and MeanDecreaseGini
total_importance <- rowSums(importance[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")])

# Set a threshold for total importance score
threshold <- 80  # Adjust as needed

# Filter features with total importance scores above the threshold
highly_relevant_features <- names(total_importance)[total_importance > threshold]

# Print highly relevant feature names
print("Highly Relevant Features:")
print(highly_relevant_features)
