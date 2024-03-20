# Install and load necessary packages
# install.packages("caTools")
# install.packages("e1071")

library(caTools)
library(e1071)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_under_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Function to perform forward selection
forward_selection <- function(data, formula, max_features) {
  selected_features <- character(0)
  features <- setdiff(names(data), "Attrition")
  for (i in 1:max_features) {
    best_accuracy <- 0
    best_feature <- NULL
    for (feature in features) {
      current_formula <- paste(formula, "+", feature)
      svm_model <- svm(formula = as.formula(current_formula), data = data, kernel = "polynomial", cost = 1)
      predictions <- predict(svm_model, newdata = data)
      accuracy <- sum(predictions == data$Attrition) / length(data$Attrition)
      if (accuracy > best_accuracy) {
        best_accuracy <- accuracy
        best_feature <- feature
      }
    }
    if (is.null(best_feature)) {
      break
    } else {
      selected_features <- c(selected_features, best_feature)
      features <- setdiff(features, best_feature)
      formula <- paste(formula, "+", best_feature)
    }
  }
  return(selected_features)
}

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Perform forward selection
selected_features <- forward_selection(train_data, formula = "Attrition ~ 1", max_features = 5)

# Include the target variable in the selected features
selected_features <- c("Attrition", selected_features)

# Subset the data with selected features
data <- data[, selected_features]

# Create an SVM model
svm_model <- svm(
  formula = Attrition ~ .,
  data = train_data,
  kernel = "polynomial",   # Alternative: "linear", "radial", "sigmoid"
  cost = 1                 # Alternative: Experiment with different cost values (e.g., 0.1, 1, 10)
)

# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data$Attrition)
print(conf_matrix)

# Calculate evaluation parameters
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