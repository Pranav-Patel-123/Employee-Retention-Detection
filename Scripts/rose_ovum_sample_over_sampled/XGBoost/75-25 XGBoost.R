# Install and load necessary packages
# install.packages(c("xgboost", "caTools"))

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
  eta = 0.1,
  max_depth = 4,  # Adjust as needed
  min_child_weight = 1.5,  # Adjust as needed
  subsample = 0.8,
  colsample_bytree = 0.8
)

#list of all the parameters
# params <- list(
#   objective = "binary:logistic",
            # objective = "reg:squarederror",  # Regression task
            # objective = "multi:softmax",  # Multiclass classification
            # objective = "rank:pairwise",  # RankNet for ranking tasks

#   eval_metric = "logloss",
            # eval_metric = "error",  # Classification error rate
            # eval_metric = "mae",  # Mean Absolute Error for regression
            # eval_metric = "auc",  # Area under the ROC curve for binary classification
            # eval_metric = "map",  # Mean Average Precision for ranking tasks

#   # Alternative parameters
#   booster = "gbtree",  # Use tree-based models
            # booster = "gblinear",  # Linear model
            # booster = "dart",  # Dropouts meet Multiple Additive Regression Trees
            # booster = "gblinear",  # Linear model
#   eta = 0.01,  # Learning rate
#   max_depth = 6,  # Maximum depth of a tree
#   subsample = 0.8,  # Subsample ratio of the training instances
#   colsample_bytree = 0.8,  # Subsample ratio of columns when constructing each tree
#   min_child_weight = 1,  # Minimum sum of instance weight (hessian) needed in a child
#   alpha = 0,  # L1 regularization term on weights
#   lambda = 1,  # L2 regularization term on weights
#   gamma = 0,  # Minimum loss reduction required to make a further partition on a leaf node
#   scale_pos_weight = 1  # Controls the balance of positive and negative weights
  
# )


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