# Install and load necessary packages
# install.packages("caTools")
# install.packages("e1071")

library(caTools)
library(e1071)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Handle missing values by removing rows with any missing values
data <- na.omit(data)

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Function to perform backward elimination
backward_elimination <- function(data, formula) {
  features <- setdiff(names(data), "Attrition")
  while (length(features) > 0) {
    best_accuracy <- 0
    best_feature <- NULL
    for (feature in features) {
      current_formula <- paste(formula, "- ", feature)
      svm_model <- tryCatch(
        svm(formula = as.formula(current_formula), data = data, kernel = "polynomial", cost = 1),
        error = function(e) NULL
      )
      if (is.null(svm_model)) {
        next
      }
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
      features <- setdiff(features, best_feature)
      formula <- paste(formula, "- ", best_feature)
    }
  }
  return(formula)
}

set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Perform backward elimination
formula <- "Attrition ~ ."
final_formula <- backward_elimination(train_data, formula)

# Create an SVM model using the final formula
svm_model <- svm(
  formula = as.formula(final_formula),
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