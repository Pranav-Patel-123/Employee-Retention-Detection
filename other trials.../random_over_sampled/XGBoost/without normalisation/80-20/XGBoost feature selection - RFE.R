# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret", "ROSE"))

library(xgboost)
library(caTools)
library(caret)
library(ROSE)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]


# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.80)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Feature selection using Recursive Feature Elimination (RFE)
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(train_data[, -1], train_data$Attrition, sizes = c(1:ncol(train_data)-1), rfeControl = ctrl)

# Get the selected features
selected_features <- names(result$optVariables)

# Select only the relevant columns
data <- data[, c("Attrition", selected_features)]

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
