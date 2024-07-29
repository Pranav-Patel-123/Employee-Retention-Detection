# Install and load necessary packages
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

# Shuffle the rows of the data
set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]


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