# Install and load necessary packages
# install.packages("caTools")
# install.packages("e1071")

library(caTools)
library(e1071)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)

# Remove outliers if found
if (length(outliers) > 0) {
  cat("Outliers detected using Z-score method in the following rows:\n")
  print(data[outliers, ])
  
  # Remove outliers
  data <- data[-outliers, ]
  
  cat("\nOutliers removed using Z-score method.\n")
} else {
  cat("No outliers detected using Z-score method.\n")
}

# Shuffle the rows of the data
set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create an SVM model
svm_model <- svm(Attrition ~ ., data = train_data, kernel = "linear", cost = 5)
#svm_model <- svm(Attrition ~ ., data = train_data, 
#                 kernel = "rbf",    # Experiment with different kernels (e.g., "linear", "poly", "sigmoid")
#                 cost = 10)         # Experiment with different cost values

# Example with radial kernel:
# svm_model <- svm(
#   formula = Attrition ~ .,
#   data = train_data,
#   kernel = "radial",   # Alternative: "linear", "polynomial", "sigmoid"
#   cost = 10            # Alternative: Experiment with different cost values (e.g., 1, 5, 20)
# )

# Example with polynomial kernel:
# svm_model <- svm(
#   formula = Attrition ~ .,
#   data = train_data,
#   kernel = "polynomial",   # Alternative: "linear", "radial", "sigmoid"
#   cost = 1                 # Alternative: Experiment with different cost values (e.g., 0.1, 1, 10)
# )

# Example with sigmoid kernel:
# svm_model <- svm(
#   formula = Attrition ~ .,
#   data = train_data,
#   kernel = "sigmoid",   # Alternative: "linear", "radial", "polynomial"
#   cost = 0.1            # Alternative: Experiment with different cost values (e.g., 0.01, 1, 10)
# )


# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

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
