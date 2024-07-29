# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")
# install.packages("caret")

library(caTools)
library(rpart)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Count rows before outlier handling
rows_before <- nrow(data)
# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)
# Remove outliers if found
if (length(outliers) > 0) {
  # Remove outliers
  data <- data[-outliers, ]} 
# Count rows after outlier handling
rows_after <- nrow(data)
# Print count of rows before and after outlier handling
cat("Rows before Z-score method of outlier handling:", rows_before, "\n")
cat("Rows after Z-score method of outlier handling:", rows_after, "\n")
# Manual scaling normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))}
# Apply normalization to numerical columns
numeric_columns <- sapply(data, is.numeric)
if (sum(numeric_columns) > 0) {
  data[, numeric_columns] <- lapply(data[, numeric_columns], normalize)
  
  cat("\nDataset normalized.\n")
} else {
  cat("\nNo numerical columns found in the dataset.\n")
}

set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Function to calculate model performance
calculate_performance <- function(features, train_data, test_data) {
  model <- rpart(Attrition ~ ., data = train_data[, c(features, "Attrition")], method = "class")
  predictions <- predict(model, newdata = test_data[, c(features, "Attrition")], type = "class")
  conf_matrix <- table(predictions, test_data$Attrition)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  return(accuracy)
}

# Initialize selected features
selected_features <- names(train_data)[-which(names(train_data) == "Attrition")]

# Initialize current best performance
best_performance <- calculate_performance(selected_features, train_data, test_data)

# Iterate over features and remove the one that degrades performance the most
while (length(selected_features) > 1) {
  performance <- sapply(selected_features, function(feature) {
    remaining_features <- setdiff(selected_features, feature)
    calculate_performance(remaining_features, train_data, test_data)
  })
  worst_feature <- selected_features[which.min(performance)]
  if (min(performance) < best_performance) {
    selected_features <- setdiff(selected_features, worst_feature)
    best_performance <- min(performance)
  } else {
    break
  }
}

# Print finally selected features
cat("Finally selected features:", selected_features, "\n")

# Implement k-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Create a decision tree model using k-fold cross-validation
model <- train(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], 
               method = "rpart", trControl = train_control)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data[, c(selected_features, "Attrition")], type = "raw")

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
