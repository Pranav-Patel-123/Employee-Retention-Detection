# Install and load necessary packages
# install.packages("caTools")
library(caTools)

# Load the dataset
data <- read.csv("rose_ovum_sample_under_sampled.csv")

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

# Perform forward selection for feature selection
selected_features <- c()  # Initialize selected features as an empty vector

# Function to evaluate model performance with selected features
evaluate_model <- function(features) {
  # Train logistic regression model with selected features
  model <- glm(Attrition ~ ., data = train_data[, c(features, "Attrition")], family = "binomial")
  
  # Make predictions on the test set with selected features
  predictions <- predict(model, newdata = test_data[, c(features, "Attrition")], type = "response")
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Confusion matrix to evaluate the model with selected features
  conf_matrix <- table(predicted_labels, test_data$Attrition)
  
  # Calculate accuracy with selected features
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  return(accuracy)
}

# Perform forward selection
while (length(selected_features) < ncol(train_data) - 1) {  # Iterate until all features are included
  best_accuracy <- 0
  best_feature <- NULL
  
  for (feature in setdiff(names(train_data), c("Attrition", selected_features))) {
    accuracy <- evaluate_model(c(selected_features, feature))
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_feature <- feature
    }
  }
  
  # Add the best feature to the selected features
  selected_features <- c(selected_features, best_feature)
  
  # Print progress
  print(paste("Selected Features:", selected_features))
  print(paste("Accuracy:", best_accuracy))
}

# Train logistic regression model with selected features
model_forward_selection <- glm(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], family = "binomial")

# Make predictions on the test set with selected features
predictions_forward_selection <- predict(model_forward_selection, newdata = test_data[, c(selected_features, "Attrition")], type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels_forward_selection <- ifelse(predictions_forward_selection > 0.5, 1, 0)

# Confusion matrix to evaluate the model with selected features
conf_matrix_forward_selection <- table(predicted_labels_forward_selection, test_data$Attrition)
print(conf_matrix_forward_selection)

# Calculate evaluation parameters
# Calculate accuracy with selected features
accuracy_forward_selection <- sum(diag(conf_matrix_forward_selection)) / sum(conf_matrix_forward_selection)
print(paste("Accuracy with selected features (Forward Selection):", accuracy_forward_selection))

# Calculate sensitivity (true positive rate) with selected features
sensitivity_forward_selection <- conf_matrix_forward_selection[2, 2] / sum(conf_matrix_forward_selection[2, ])
print(paste("Sensitivity with selected features (Forward Selection):", sensitivity_forward_selection))

# Calculate precision (positive predictive value) with selected features
precision_forward_selection <- conf_matrix_forward_selection[2, 2] / sum(conf_matrix_forward_selection[, 2])
print(paste("Precision with selected features (Forward Selection):", precision_forward_selection))

# Calculate specificity (true negative rate) with selected features
specificity_forward_selection <- conf_matrix_forward_selection[1, 1] / sum(conf_matrix_forward_selection[1, ])
print(paste("Specificity with selected features (Forward Selection):", specificity_forward_selection))
