# Install and load necessary packages
# install.packages("randomForest")
library(randomForest)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

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

set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Train Random Forest model to get feature importance
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = train_data,
  ntree = 600,          # Number of trees in the forest
  importance = TRUE     # Compute and store variable importance
)

# Get feature importance scores
importance_scores <- importance(model_rf)

# Sort features by importance scores
sorted_importance <- importance_scores[order(importance_scores[, "MeanDecreaseGini"], decreasing = TRUE), ]

# Select top features based on importance scores
top_features <- rownames(sorted_importance)[1:10]  # Select top 10 features (adjust as needed)

# Subset train and test data with selected features
train_data_selected <- train_data[, c(top_features, "Attrition")]
test_data_selected <- test_data[, c(top_features, "Attrition")]

# Train Random Forest model with selected features
model_rf_selected <- randomForest(
  formula = Attrition ~ .,
  data = train_data_selected,
  ntree = 600,          # Number of trees in the forest
  mtry = sqrt(length(top_features)),  # Number of variables randomly sampled as candidates at each split
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE     # Compute and store variable importance
)

# Make predictions on the test set with selected features
predictions_rf_selected <- predict(model_rf_selected, newdata = test_data_selected)

# Confusion matrix to evaluate the model with selected features
conf_matrix_rf_selected <- table(predictions_rf_selected, test_data_selected$Attrition)
print(conf_matrix_rf_selected)

# Calculate accuracy with selected features
accuracy_rf_selected <- sum(diag(conf_matrix_rf_selected)) / sum(conf_matrix_rf_selected)
print(paste("Accuracy with selected features:", accuracy_rf_selected))

# Calculate sensitivity (true positive rate)
sensitivity_rf_selected <- conf_matrix_rf_selected[2, 2] / sum(conf_matrix_rf_selected[2, ])
print(paste("Sensitivity with selected features:", sensitivity_rf_selected))

# Calculate specificity (true negative rate)
specificity_rf_selected <- conf_matrix_rf_selected[1, 1] / sum(conf_matrix_rf_selected[1, ])
print(paste("Specificity with selected features:", specificity_rf_selected))

# Calculate precision (positive predictive value)
precision_rf_selected <- conf_matrix_rf_selected[2, 2] / sum(conf_matrix_rf_selected[, 2])
print(paste("Precision with selected features:", precision_rf_selected))