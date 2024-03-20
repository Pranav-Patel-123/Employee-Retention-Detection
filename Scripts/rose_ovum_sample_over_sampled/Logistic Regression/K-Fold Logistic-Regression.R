# Install and load necessary packages
#install.packages("caTools")
#install.packages("glmnet")

library(caTools)
library(glmnet)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Set the seed for reproducibility
set.seed(123)

# K-fold cross-validation with 10 folds
k <- 10
folds <- sample(1:k, nrow(data), replace = TRUE)
cv_conf_matrix <- matrix(0, nrow = 2, ncol = 2)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Create training and testing sets for the current fold
  train_data <- data[folds != i, ]
  test_data <- data[folds == i, ]
  
  # Create a logistic regression model
  model <- glm(Attrition ~ ., data = train_data, family = "binomial")
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = test_data, type = "response")
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Update the confusion matrix for the current fold
  cv_conf_matrix <- cv_conf_matrix + table(predicted_labels, test_data$Attrition)
}

# Print the overall confusion matrix for cross-validation
print(cv_conf_matrix)

# Calculate overall accuracy for cross-validation
cv_accuracy <- sum(diag(cv_conf_matrix)) / sum(cv_conf_matrix)
print(paste("Overall Accuracy (Cross-Validation):", cv_accuracy))

# Calculate overall sensitivity (true positive rate) for cross-validation
cv_sensitivity <- cv_conf_matrix[2, 2] / sum(cv_conf_matrix[2, ])
print(paste("Overall Sensitivity (Cross-Validation):", cv_sensitivity))

# Calculate overall precision (positive predictive value) for cross-validation
cv_precision <- cv_conf_matrix[2, 2] / sum(cv_conf_matrix[, 2])
print(paste("Overall Precision (Cross-Validation):", cv_precision))

# Calculate overall specificity (true negative rate) for cross-validation
cv_specificity <- cv_conf_matrix[1, 1] / sum(cv_conf_matrix[1, ])
print(paste("Overall Specificity (Cross-Validation):", cv_specificity))
