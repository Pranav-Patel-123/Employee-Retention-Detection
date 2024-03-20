# Install and load necessary packages
# install.packages("caTools")
# install.packages("glmnet")
# install.packages("randomForest")

library(caTools)
library(glmnet)
library(randomForest)

# Load the dataset
data <- read.csv("random_under_sampled.csv")

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

# Train a Random Forest model for feature selection
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = train_data,
  ntree = 600,           # Number of trees in the forest
  importance = TRUE      # Compute variable importance
)

# Extract feature importance scores
importance_scores <- importance(model_rf)

# Select top features based on their importance scores
top_features <- rownames(importance_scores)[order(importance_scores[, "MeanDecreaseGini"], decreasing = TRUE)][1:10]

# Filter train and test data with selected top features
train_data_rf <- train_data[, c(top_features, "Attrition")]
test_data_rf <- test_data[, c(top_features, "Attrition")]

# Train a logistic regression model using selected features
model_logistic <- glm(Attrition ~ ., data = train_data_rf, family = "binomial")

# Make predictions on the test set
predictions <- predict(model_logistic, newdata = test_data_rf, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the model
conf_matrix <- table(predicted_labels, test_data_rf$Attrition)
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
