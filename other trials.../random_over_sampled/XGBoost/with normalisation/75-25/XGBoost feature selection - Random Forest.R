# Install and load necessary packages
# install.packages(c("xgboost", "caTools", "caret", "randomForest"))

library(xgboost)
library(caTools)
library(caret)
library(randomForest)

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

# Perform feature selection using Random Forest
rf_model <- randomForest(Attrition ~ ., data = train_data, importance = TRUE, ntree = 500)

# Extract feature importance scores
importance_scores <- importance(rf_model)

# Select top features based on importance scores
top_features <- rownames(importance_scores)[order(importance_scores[, 1], decreasing = TRUE)[1:10]]

# Subset the data with selected features
data <- data[, c("Attrition", top_features)]

# Convert data to matrix format for XGBoost
train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)

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

# Train the XGBoost model using the subset of data with Random Forest-selected features
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
