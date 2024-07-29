# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret"))

library(xgboost)
library(caTools)
library(caret)

# Load the dataset
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

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
  max_depth = 4,  # Adjust as needed
  min_child_weight = 1.5,  # Adjust as needed
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Set the number of folds (k=10)
num_folds <- 10

# Explicitly set train control for cross-validation
train_control <- trainControl(
  method = "cv",
  number = num_folds
)

# Perform k-fold cross-validation with xgb.cv
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nfold = num_folds,
  nrounds = 100,  # This is the maximum number of boosting rounds
  early_stopping_rounds = 10,  # Set a value for early stopping
  verbose = 0,
  folds = createFolds(as.factor(train_data$Attrition), k = num_folds),
  stratified = TRUE
)

# Get the optimal number of boosting rounds
optimal_rounds <- which.min(cv_results$test_error_mean)

# Train the XGBoost model using the optimal number of rounds
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 50, verbose = 0)

# Make predictions on the test set
predictions <- predict(xgb_model, dtest)

# Convert probabilities to binary predictions (0 or 1)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the model
conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Sensitivity:", sensitivity))

# Calculate precision (positive predictive value)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste("Precision:", precision))

# Calculate specificity (true negative rate)
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
print(paste("Specificity:", specificity))
