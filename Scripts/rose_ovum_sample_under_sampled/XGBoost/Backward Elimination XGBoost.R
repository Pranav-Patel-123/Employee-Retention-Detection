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

# Backward elimination
selected_features_backward <- selected_features
best_accuracy <- 0
best_result <- NULL

for (i in length(selected_features):1) {
  candidate_features <- selected_features_backward[-i]
  
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
    eta = 0.3,
    max_depth = 20,  # Adjust as needed
    min_child_weight = 10,  # Adjust as needed
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train the XGBoost model
  xgb_model <- xgboost(params = params, data = dtrain, nrounds = 200, verbose = 0)
  
  # Make predictions on the test set
  predictions <- predict(xgb_model, dtest)
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.6, 1, 0)
  
  # Confusion matrix to evaluate the model
  conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
  
  # Calculate evaluation parameters
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  
  # Update selected features and best result if accuracy improves
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    selected_features_backward <- candidate_features
    best_result <- data.frame(
      Features = paste(candidate_features, collapse = ", "),
      Accuracy = accuracy,
      Sensitivity = sensitivity,
      Precision = precision,
      Specificity = specificity
    )
  }
}

# Print the best result (Backward Elimination)
print("Best Result (Backward Elimination):")
print(best_result)
