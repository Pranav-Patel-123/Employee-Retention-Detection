# Install and load necessary packages
# install.packages("caret")
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

library(e1071)

# Identify outliers using One-Class SVM
svm_model <- svm(data[, sapply(data, is.numeric)], type = "one-classification", nu = 0.1)
outliers <- which(predict(svm_model, data[, sapply(data, is.numeric)]) == -1)

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using One-Class SVM method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data_cleaned <- data[-outliers, ]
  
  cat("\nOutliers removed using One-Class SVM method.\n")
} else {
  cat("No outliers detected using One-Class SVM method.\n")
}

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

# Shuffle the rows of the data
set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]


# Define control parameters for RFE
ctrl <- rfeControl(functions = rfFuncs,  # Use Random Forest for feature selection
                   method = "cv",        # Use cross-validation
                   number = 10)          # Number of folds for cross-validation

# Perform Recursive Feature Elimination (RFE)
rfe_result <- rfe(x = data[, -which(names(data) == "Attrition")],  # Features
                  y = data$Attrition,                             # Target variable
                  sizes = c(1:10),                                 # Number of features to consider at each iteration
                  rfeControl = ctrl)

# Get the selected features
selected_features <- predictors(rfe_result)

# Subset train and test data with selected features
train_data_selected <- train_data[, c(selected_features, "Attrition")]
test_data_selected <- test_data[, c(selected_features, "Attrition")]

# Train Random Forest model with selected features
model_rf_selected <- randomForest(
  formula = Attrition ~ .,
  data = train_data_selected,
  ntree = 600,          # Number of trees in the forest
  mtry = sqrt(length(selected_features)),  # Number of variables randomly sampled as candidates at each split
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE     # Compute and store variable importance
)

# Make predictions on the test set with selected features
predictions_rf_selected <- predict(model_rf_selected, newdata = test_data_selected)

# Confusion matrix to evaluate the model with selected features
conf_matrix_rf_selected <- table(predictions_rf_selected, test_data_selected$Attrition)
print(conf_matrix_rf_selected)

# Calculate evaluation parameters
# Calculate accuracy with selected features
accuracy_rf_selected <- sum(diag(conf_matrix_rf_selected)) / sum(conf_matrix_rf_selected)
print(paste("Accuracy with selected features:", accuracy_rf_selected))

# Calculate sensitivity (true positive rate) with selected features
sensitivity_rf_selected <- conf_matrix_rf_selected[2, 2] / sum(conf_matrix_rf_selected[2, ])
print(paste("Sensitivity with selected features:", sensitivity_rf_selected))

# Calculate specificity (true negative rate) with selected features
specificity_rf_selected <- conf_matrix_rf_selected[1, 1] / sum(conf_matrix_rf_selected[1, ])
print(paste("Specificity with selected features:", specificity_rf_selected))

# Calculate precision (positive predictive value) with selected features
precision_rf_selected <- conf_matrix_rf_selected[2, 2] / sum(conf_matrix_rf_selected[, 2])
print(paste("Precision with selected features:", precision_rf_selected))
