# Install and load necessary packages
# install.packages("caTools")
# install.packages("e1071")

library(caTools)
library(e1071)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Convert data to matrix format for SVM
data_matrix <- model.matrix(Attrition ~ . - 1, data = data)

# Specify the number of folds (k=10)
num_folds <- 10

# Create an empty confusion matrix to aggregate results
aggregate_conf_matrix <- matrix(0, nrow = 2, ncol = 2)

# Perform k-fold cross-validation
for (fold in 1:num_folds) {
  # Create folds
  folds <- createFolds(as.factor(data$Attrition), k = num_folds, list = TRUE)
  test_indices <- unlist(folds[fold])
  
  # Split data into training and testing sets
  train_data <- data[-test_indices, ]
  test_data <- data[test_indices, ]
  
  # Create an SVM model
  svm_model <- svm(Attrition ~ ., data = train_data, kernel = "linear", cost = 1)
  # Example with cost parameter set to 2 (you can adjust this value)
  #svm_model <- svm(Attrition ~ ., data = train_data, kernel = "linear", cost = 2)
  # Example with a polynomial kernel (you can adjust the degree)
  #svm_model <- svm(Attrition ~ ., data = train_data, kernel = "polynomial", degree = 3, cost = 2)
  # Example with the radial kernel and adjusted gamma (you can adjust this value)
  #svm_model <- svm(Attrition ~ ., data = train_data, kernel = "radial", gamma = 0.1, cost = 2)
  # Example with a polynomial kernel and adjusted degree (you can adjust this value)
  #svm_model <- svm(Attrition ~ ., data = train_data, kernel = "polynomial", degree = 4, cost = 2)
  # Example of tuning parameters using caret's tune function
  #tune_results <- tune(svm, Attrition ~ ., data = train_data, kernel = "radial", ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1)))
  
  
  # Make predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
  # Confusion matrix to evaluate the model
  conf_matrix <- table(predictions, test_data$Attrition)
  
  # Aggregate confusion matrices
  aggregate_conf_matrix <- aggregate_conf_matrix + conf_matrix
}

# Calculate average confusion matrix and performance metrics
avg_conf_matrix <- aggregate_conf_matrix / num_folds

# Print the average confusion matrix
print(avg_conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(avg_conf_matrix)) / sum(avg_conf_matrix)
print(paste("Accuracy:", accuracy))

# Calculate sensitivity (true positive rate)
sensitivity <- avg_conf_matrix[2, 2] / sum(avg_conf_matrix[2, ])
print(paste("Sensitivity:", sensitivity))

# Calculate precision (positive predictive value)
precision <- avg_conf_matrix[2, 2] / sum(avg_conf_matrix[, 2])
print(paste("Precision:", precision))

# Calculate specificity (true negative rate)
specificity <- avg_conf_matrix[1, 1] / sum(avg_conf_matrix[1, ])
print(paste("Specificity:", specificity))
