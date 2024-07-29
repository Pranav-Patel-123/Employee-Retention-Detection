# Install and load necessary packages
# install.packages(c("randomForest", "caTools", "caret"))

library(randomForest)
library(caret)
library(caTools)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

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


# Specify the number of folds (k=10)
num_folds <- 10

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.70)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Feature selection using Chi-Square Test
chi_square_test <- apply(train_data[, categorical_columns], 2, function(x) chisq.test(table(x, train_data$Attrition))$statistic)
selected_features <- names(chi_square_test)[chi_square_test > 3.841]  # Threshold for significance, adjust as needed

# Select only the relevant columns
train_data <- train_data[, c("Attrition", selected_features)]
test_data <- test_data[, c("Attrition", selected_features)]

# Create a train control for k-fold cross-validation within the 75% training set
train_control <- trainControl(
  method = "cv",                # Use k-fold cross-validation
  number = num_folds,           # Number of folds
  savePredictions = "final",    # Save final predictions on the test set
  classProbs = TRUE,            # Enable class probabilities
  index = createMultiFolds(train_data$Attrition, k = num_folds, times = 1)  # k-fold within the 75% training set
)

# Create a random forest model using train function
model_rf <- train(
  x = train_data[, -which(names(train_data) == "Attrition")],
  y = train_data$Attrition,
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(mtry = sqrt(ncol(train_data))),  # Number of variables randomly sampled as candidates at each split
  nodesize = 10,               # Minimum size of terminal nodes
  importance = TRUE            # Compute and store variable importance
)

# Print the model information
print(model_rf)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test_data[, -which(names(test_data) == "Attrition")])

# Confusion matrix to evaluate the model
conf_matrix_rf <- table(predictions_rf, test_data$Attrition)
print(conf_matrix_rf)

# Calculate accuracy
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Accuracy:", accuracy_rf))

# Calculate sensitivity (true positive rate)
sensitivity_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ])
print(paste("Sensitivity:", sensitivity_rf))

# Calculate precision (positive predictive value)
precision_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2])
print(paste("Precision:", precision_rf))

# Calculate specificity (true negative rate)
specificity_rf <- conf_matrix_rf[1, 1] / sum(conf_matrix_rf[1, ])
print(paste("Specificity:", specificity_rf))
