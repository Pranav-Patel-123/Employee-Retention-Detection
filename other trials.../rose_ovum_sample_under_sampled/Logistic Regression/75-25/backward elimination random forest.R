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

# Function to perform backward elimination for feature selection
perform_backward_elimination <- function(data) {
  all_features <- names(data)[!names(data) %in% "Attrition"]
  selected_features <- all_features
  while (length(selected_features) > 1) {
    model <- glm(Attrition ~ ., data = data[, c(selected_features, "Attrition")], family = "binomial")
    p_values <- summary(model)$coefficients[, 4][-1]  # Exclude intercept
    least_significant_feature <- names(p_values)[which.max(p_values)]
    if (max(p_values) > 0.05) {
      break  # Exit loop if no significant features left
    }
    selected_features <- selected_features[selected_features != least_significant_feature]
  }
  return(selected_features)
}

# Perform backward elimination for feature selection
selected_features <- perform_backward_elimination(train_data)

# Train logistic regression model with selected features
model_backward_elimination <- glm(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], family = "binomial")

# Make predictions on the test set with selected features
predictions_backward_elimination <- predict(model_backward_elimination, newdata = test_data[, c(selected_features, "Attrition")], type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels_backward_elimination <- ifelse(predictions_backward_elimination > 0.5, 1, 0)

# Confusion matrix to evaluate the model with selected features
conf_matrix_backward_elimination <- table(predicted_labels_backward_elimination, test_data$Attrition)
print(conf_matrix_backward_elimination)

# Calculate evaluation parameters
# Calculate accuracy with selected features
accuracy_backward_elimination <- sum(diag(conf_matrix_backward_elimination)) / sum(conf_matrix_backward_elimination)
print(paste("Accuracy with selected features (Backward Elimination):", accuracy_backward_elimination))

# Calculate sensitivity (true positive rate) with selected features
sensitivity_backward_elimination <- conf_matrix_backward_elimination[2, 2] / sum(conf_matrix_backward_elimination[2, ])
print(paste("Sensitivity with selected features (Backward Elimination):", sensitivity_backward_elimination))

# Calculate precision (positive predictive value) with selected features
precision_backward_elimination <- conf_matrix_backward_elimination[2, 2] / sum(conf_matrix_backward_elimination[, 2])
print(paste("Precision with selected features (Backward Elimination):", precision_backward_elimination))

# Calculate specificity (true negative rate) with selected features
specificity_backward_elimination <- conf_matrix_backward_elimination[1, 1] / sum(conf_matrix_backward_elimination[1, ])
print(paste("Specificity with selected features (Backward Elimination):", specificity_backward_elimination))
