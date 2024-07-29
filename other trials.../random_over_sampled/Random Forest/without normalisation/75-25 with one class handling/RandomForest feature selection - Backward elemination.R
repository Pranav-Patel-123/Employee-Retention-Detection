# Install and load necessary packages
# install.packages(c("randomForest", "caTools", "caret"))

library(randomForest)
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

# Backward elimination
selected_features_backward <- selected_features
best_accuracy <- 0
best_result <- NULL

for (i in length(selected_features):1) {
  candidate_features <- selected_features_backward[-i]
  
  # Select only the relevant columns
  data_subset <- data[, c("Attrition", candidate_features)]
  
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
  split <- sample.split(data_subset$Attrition, SplitRatio = 0.75)
  train_data <- subset(data_subset, split == TRUE)
  test_data <- subset(data_subset, split == FALSE)
  
  # Create a random forest model
  model_rf <- randomForest(
    x = train_data[, -which(names(train_data) == "Attrition")],
    y = train_data$Attrition,
    ntree = 200,                # Number of trees
    nodesize = 10               # Minimum size of terminal nodes
  )
  
  # Make predictions on the test set
  predictions_rf <- predict(model_rf, newdata = test_data)
  
  # Confusion matrix to evaluate the model
  conf_matrix_rf <- table(predictions_rf, test_data$Attrition)
  
  # Calculate evaluation parameters
  accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
  sensitivity_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ])
  precision_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2])
  specificity_rf <- conf_matrix_rf[1, 1] / sum(conf_matrix_rf[1, ])
  
  # Update selected features and best result if accuracy improves
  if (accuracy_rf > best_accuracy) {
    best_accuracy <- accuracy_rf
    selected_features_backward <- candidate_features
    best_result <- data.frame(
      Features = paste(candidate_features, collapse = ", "),
      Accuracy = accuracy_rf,
      Sensitivity = sensitivity_rf,
      Precision = precision_rf,
      Specificity = specificity_rf
    )
  }
}

# Print the best result (Backward Elimination with Random Forest)
print("Best Result (Backward Elimination with Random Forest):")
print(best_result)
