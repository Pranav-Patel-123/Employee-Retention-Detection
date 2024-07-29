# Load necessary packages if not already loaded
# install.packages(c("xgboost", "caTools", "caret"))
library(xgboost)
library(caTools)
library(caret)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Convert data to numeric for correlation calculation
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Perform feature selection based on correlation
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.8)
selected_features <- names(numeric_data)[-1][!names(numeric_data)[-1] %in% correlated_features]

# Select only the relevant columns
data <- data[, c("Attrition", selected_features)]

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

# Set seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
k <- 10

# Create indices for cross-validation
folds <- createFolds(data$Attrition, k = k, list = TRUE, returnTrain = FALSE)

# Initialize vectors to store evaluation metrics
accuracies <- numeric(k)
sensitivities <- numeric(k)
precisions <- numeric(k)
specificities <- numeric(k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Split data into training and testing sets for this fold
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # Convert data to matrix format for XGBoost
  train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
  test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(train_data$Attrition) - 1)
  dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(test_data$Attrition) - 1)
  
  # Specify XGBoost parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1,
    max_depth = 10,  # Adjust as needed
    min_child_weight = 5,  # Adjust as needed
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train the XGBoost model
  xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)
  
  # Make predictions on the test set
  predictions <- predict(xgb_model, dtest)
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Confusion matrix to evaluate the model
  conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
  
  # Calculate evaluation metrics
  accuracies[i] <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivities[i] <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  precisions[i] <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  specificities[i] <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
}

# Print mean evaluation metrics
cat("\nMean Accuracy:", mean(accuracies))
cat("\nMean Sensitivity:", mean(sensitivities))
cat("\nMean Precision:", mean(precisions))
cat("\nMean Specificity:", mean(specificities))
