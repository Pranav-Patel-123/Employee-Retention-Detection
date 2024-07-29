# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret"))

library(xgboost)
library(caTools)
library(caret)

# Load the dataset
data <- read.csv("random_under_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Create contingency tables for Chi-Square Test
contingency_tables <- lapply(categorical_columns, function(column) {
  table_data <- table(data$Attrition, data[, column])
  chisq.test(table_data)$p.value
})

# Select features based on a significance level (e.g., 0.05)
selected_features <- categorical_columns[contingency_tables < 0.5]

# Add selected categorical features to the dataset
data <- data[, c("Attrition", selected_features)]

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# ...

# Convert data to matrix format for XGBoost
train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)

dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(train_data$Attrition) - 1)
dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(test_data$Attrition) - 1)

# ...

# Specify XGBoost parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.6,  # Adjust learning rate as needed
  max_depth = 100,  # Adjust as needed
  min_child_weight = 3,  # Adjust as needed
  subsample = 0.8,  # Adjust as needed
  colsample_bytree = 0.8  # Adjust as needed
)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 500, verbose = 0)

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
