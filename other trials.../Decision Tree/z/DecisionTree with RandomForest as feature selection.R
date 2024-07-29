# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("caret")

library(caTools)
library(rpart)
library(randomForest)
library(caret)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert specified categorical columns to factors
data[, categorical_columns] <- lapply(data[, categorical_columns], as.factor)

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

# Train a Random Forest model to determine feature importance
rf_model <- randomForest(Attrition ~ ., data = train_data, ntree = 500)

# Extract variable importance scores
importance_scores <- importance(rf_model)

# Select the top features based on importance scores (e.g., top 10 features)
top_features <- names(importance_scores[order(-importance_scores), ][1:10])

# Filter train and test data with top features
train_data_rf <- train_data[, c(top_features, "Attrition")]
test_data_rf <- test_data[, c(top_features, "Attrition")]

# Print finally selected features for training
cat("Finally selected features:", top_features, "\n")

# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Ensure 'Attrition' is treated as a factor for classification
train_data_rf$Attrition <- as.factor(train_data_rf$Attrition)
test_data_rf$Attrition <- as.factor(test_data_rf$Attrition)

# Create a decision tree model using 10-fold cross-validation
model <- train(Attrition ~ ., data = train_data_rf, method = "rpart", trControl = train_control)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data_rf, type = "raw")

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data_rf$Attrition)
print(conf_matrix)

# Calculate evaluation parameters
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
