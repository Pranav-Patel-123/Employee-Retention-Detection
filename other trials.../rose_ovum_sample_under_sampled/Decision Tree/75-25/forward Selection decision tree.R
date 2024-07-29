# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")

library(caTools)
library(rpart)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_under_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Initialize an empty set of selected features
selected_features <- c()

# Initialize a variable to keep track of the best accuracy
best_accuracy <- 0

# Function to calculate accuracy
calculate_accuracy <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data, type = "class")
  conf_matrix <- table(predictions, test_data$Attrition)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  return(accuracy)
}

# Iterate through each feature
for (feature in setdiff(names(train_data), "Attrition")) {
  # Add the feature to the set of selected features
  selected_features <- c(selected_features, feature)
  
  # Create a decision tree model using the selected features
  model <- rpart(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "class")
  
  # Calculate accuracy using the test set
  accuracy <- calculate_accuracy(model, test_data[, c(selected_features, "Attrition")])
  
  # Check if the current set of features improves the accuracy
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
  } else {
    # Remove the last added feature if it doesn't improve the accuracy
    selected_features <- selected_features[-length(selected_features)]
  }
}

# Final decision tree model using the selected features
final_model <- rpart(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "class")

# Confusion matrix to evaluate the final model
final_conf_matrix <- table(predict(final_model, newdata = test_data[, c(selected_features, "Attrition")], type = "class"), test_data$Attrition)
print(final_conf_matrix)

# Calculate evaluation parameters for the final model
final_accuracy <- sum(diag(final_conf_matrix)) / sum(final_conf_matrix)
final_sensitivity <- final_conf_matrix[2, 2] / sum(final_conf_matrix[2, ])
final_precision <- final_conf_matrix[2, 2] / sum(final_conf_matrix[, 2])
final_specificity <- final_conf_matrix[1, 1] / sum(final_conf_matrix[1, ])

# Print evaluation parameters for the final model
print(paste("Final Accuracy:", final_accuracy))
print(paste("Final Sensitivity:", final_sensitivity))
print(paste("Final Precision:", final_precision))
print(paste("Final Specificity:", final_specificity))
