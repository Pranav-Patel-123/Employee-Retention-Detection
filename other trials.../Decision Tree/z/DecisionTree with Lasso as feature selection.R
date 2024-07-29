library(caTools)
library(glmnet)
library(caret)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Manual scaling normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to relevant numerical columns
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

# Define predictors and response variable
x <- as.matrix(train_data[, !names(train_data) %in% "Attrition"])
y <- as.vector(train_data$Attrition)

# Fit Lasso model
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Extract selected features based on Lasso regularization
lasso_selected_features <- coef(lasso_model, s = "lambda.min")[-1, ]
lasso_selected_feature_indices <- which(lasso_selected_features != 0)
lasso_selected_feature_names <- names(train_data)[lasso_selected_feature_indices]

# Filter train and test data with selected features for Lasso
train_data_lasso <- train_data[, c(lasso_selected_feature_indices, which(names(train_data) == "Attrition"))]
test_data_lasso <- test_data[, c(lasso_selected_feature_indices, which(names(test_data) == "Attrition"))]

# Fit Ridge model
ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0)

# Extract selected features based on Ridge regularization
ridge_selected_features <- coef(ridge_model, s = "lambda.min")[-1, ]
ridge_selected_feature_indices <- which(ridge_selected_features != 0)
ridge_selected_feature_names <- names(train_data)[ridge_selected_feature_indices]

# Filter train and test data with selected features for Ridge
train_data_ridge <- train_data[, c(ridge_selected_feature_indices, which(names(train_data) == "Attrition"))]
test_data_ridge <- test_data[, c(ridge_selected_feature_indices, which(names(test_data) == "Attrition"))]

# Define training control for 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train a decision tree model with Lasso-selected features using 10-fold cross-validation
model_lasso <- train(Attrition ~ ., data = train_data_lasso, method = "rpart", trControl = train_control)

# Train a decision tree model with Ridge-selected features using 10-fold cross-validation
model_ridge <- train(Attrition ~ ., data = train_data_ridge, method = "rpart", trControl = train_control)

# Make predictions on the test set using the Lasso-selected features model
predictions_lasso <- predict(model_lasso, newdata = test_data_lasso, type = "raw")
# Make predictions on the test set using the Ridge-selected features model
predictions_ridge <- predict(model_ridge, newdata = test_data_ridge, type = "raw")

# Evaluate Lasso model
conf_matrix_lasso <- confusionMatrix(predictions_lasso, test_data_lasso$Attrition)
accuracy_lasso <- conf_matrix_lasso$overall['Accuracy']
sensitivity_lasso <- conf_matrix_lasso$byClass['Sensitivity']
precision_lasso <- conf_matrix_lasso$byClass['Precision']
specificity_lasso <- conf_matrix_lasso$byClass['Specificity']

# Evaluate Ridge model
conf_matrix_ridge <- confusionMatrix(predictions_ridge, test_data_ridge$Attrition)
accuracy_ridge <- conf_matrix_ridge$overall['Accuracy']
sensitivity_ridge <- conf_matrix_ridge$byClass['Sensitivity']
precision_ridge <- conf_matrix_ridge$byClass['Precision']
specificity_ridge <- conf_matrix_ridge$byClass['Specificity']

# Print selected features for Lasso
cat("Selected Features for Lasso (L1) Regularization:\n")
cat(lasso_selected_feature_names, "\n")

# Print selected features for Ridge
cat("\nSelected Features for Ridge (L2) Regularization:\n")
cat(ridge_selected_feature_names, "\n")

# Print evaluation results for Lasso
cat("\nEvaluation Results for Lasso (L1) Regularization:\n")
cat("Accuracy:", accuracy_lasso, "\n")
cat("Sensitivity (True Positive Rate):", sensitivity_lasso, "\n")
cat("Precision (Positive Predictive Value):", precision_lasso, "\n")
cat("Specificity (True Negative Rate):", specificity_lasso, "\n")

# Print evaluation results for Ridge
cat("\nEvaluation Results for Ridge (L2) Regularization:\n")
cat("Accuracy:", accuracy_ridge, "\n")
cat("Sensitivity (True Positive Rate):", sensitivity_ridge, "\n")
cat("Precision (Positive Predictive Value):", precision_ridge, "\n")
cat("Specificity (True Negative Rate):", specificity_ridge, "\n")
