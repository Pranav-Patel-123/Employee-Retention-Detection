# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret"))

library(xgboost)
library(caTools)
library(caret)

# Load the dataset
data <- read.csv("rose_ovum_sample_under_sampled.csv")

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

# Forward selection
selected_features_forward <- c()
best_accuracy <- 0
evaluation_results <- data.frame(Iteration = numeric(0), Features = character(0), Accuracy = numeric(0))

for (i in 1:length(selected_features)) {
  candidate_features <- c(selected_features_forward, selected_features[i])
  
  # Select only the relevant columns
  data_subset <- data[, c("Attrition", candidate_features)]
  
  # Set the seed for reproducibility
  set.seed(123)
  
  # Split the data into training (75%) and testing (25%) sets
  split <- sample.split(data_subset$Attrition, SplitRatio = 0.75)
  train_data <- subset(data_subset, split == TRUE)
  test_data <- subset(data_subset, split == FALSE)
  
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
  xgb_model <- xgboost(params = params, data = dtrain, nrounds = 200, verbose = 0)
  
  # Make predictions on the test set
  predictions <- predict(xgb_model, dtest)
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Confusion matrix to evaluate the model
  conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
  
  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Update selected features if accuracy improves
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    selected_features_forward <- candidate_features
  }
  
  # Store evaluation results for each iteration
  evaluation_results <- rbind(evaluation_results, data.frame(Iteration = i, Features = paste(candidate_features, collapse = ", "), Accuracy = accuracy))
}

# Print the selected features
print("Selected Features (Forward Selection):")
print(selected_features_forward)

# Print the best accuracy
print(paste("Best Accuracy:", best_accuracy))

# Print evaluation results for each iteration
print("Evaluation Results:")
print(evaluation_results)

# Evaluate the final model with the selected features
final_model <- xgboost(params = params, data = dtrain, nrounds = 150, verbose = 0)

# Make predictions on the test set using the final model
final_predictions <- predict(final_model, dtest)

# Convert probabilities to binary predictions (0 or 1)
final_predicted_labels <- ifelse(final_predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the final model
final_conf_matrix <- table(final_predicted_labels, as.numeric(test_data$Attrition) - 1)

# Calculate and print accuracy
final_accuracy <- sum(diag(final_conf_matrix)) / sum(final_conf_matrix)
print(paste("Final Accuracy:", final_accuracy))

# Calculate and print sensitivity (true positive rate)
final_sensitivity <- final_conf_matrix[2, 2] / sum(final_conf_matrix[2, ])
print(paste("Final Sensitivity:", final_sensitivity))

# Calculate and print precision (positive predictive value)
final_precision <- final_conf_matrix[2, 2] / sum(final_conf_matrix[, 2])
print(paste("Final Precision:", final_precision))

# Calculate and print specificity (true negative rate)
final_specificity <- final_conf_matrix[1, 1] / sum(final_conf_matrix[1, ])
print(paste("Final Specificity:", final_specificity))
