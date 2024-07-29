# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret", "glmnet"))

library(xgboost)
library(caTools)
library(caret)
library(glmnet)

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


set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]


# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Convert data to matrix format for XGBoost
train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)

# Perform LASSO feature selection
x <- as.matrix(train_matrix[, -1])  # Exclude the response variable
y <- as.numeric(train_data$Attrition) - 1

lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Extract the selected features
lasso_selected_features <- coef(lasso_model, s = lasso_model$lambda.min)
lasso_selected_features <- as.numeric(names(lasso_selected_features)[lasso_selected_features != 0])

# Include the intercept (if present)
if (length(lasso_selected_features) > 0) {
  lasso_selected_features <- lasso_selected_features + 1
}

# Subset the data with selected features
lasso_selected_data <- data[, c("Attrition", colnames(train_matrix)[lasso_selected_features])]

# Continue with the rest of your code...

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

# Specify XGBoost parameters
# params <- list(
#   objective = "binary:logistic",
#   eval_metric = "logloss",
#   eta = 0.05,  # Alternative: 0.01, 0.1, 0.2, etc.
#   max_depth = 8,  # Alternative: 4, 6, 10, etc.
#   min_child_weight = 1,  # Alternative: 3, 5, 10, etc.
#   subsample = 0.7,  # Alternative: 0.8, 0.9, 1.0, etc.
#   colsample_bytree = 0.8  # Alternative: 0.9, 1.0, etc.
# )

# # Perform LASSO feature selection
# lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial", nfolds = 10)  # Alternative: Adjust nfolds as needed

# # Train the XGBoost model using the subset of data with LASSO-selected features
# xgb_model <- xgboost(params = params, data = dtrain, nrounds = 200, verbose = 0)  # Alternative: Adjust nrounds as needed





# Train the XGBoost model using the subset of data with LASSO-selected features
dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(train_data$Attrition) - 1)
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)

# Make predictions on the test set
dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(test_data$Attrition) - 1)
predictions <- predict(xgb_model, dtest)

# Convert probabilities to binary predictions (0 or 1)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the model
conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
print(conf_matrix)

# Calculate and print evaluation metrics (accuracy, sensitivity, precision, specificity)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Precision:", precision))
print(paste("Specificity:", specificity))
