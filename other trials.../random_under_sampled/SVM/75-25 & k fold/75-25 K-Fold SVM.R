# Install and load necessary packages
# install.packages("caTools")
# install.packages("e1071")
# install.packages("caret")

library(caTools)
library(e1071)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
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

# Convert data to matrix format for SVM
train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)

# Specify the number of folds (k=10)
num_folds <- 10

# Explicitly set train control for cross-validation
train_control <- trainControl(
  method = "cv",
  number = num_folds,
  savePredictions = "final",  # Save final predictions on the test set
  classProbs = TRUE   # Enable class probabilities
)

# Create an SVM model using train function with k-fold cross-validation
svm_model <- train(
  x = train_matrix,
  y = as.factor(train_data$Attrition),
  method = "svmLinear",
  trControl = train_control,
  preProcess = c("center", "scale")  # Optional: Apply feature scaling
)

# Print the model information
print(svm_model)

# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_matrix)

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data$Attrition)
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
