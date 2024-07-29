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

# Print finally selected features
cat("Finally selected features:", selected_features, "\n")

# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)
# Create a decision tree model using 10-fold cross-validation
model <- train(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "rpart", trControl = train_control)

# Confusion matrix to evaluate the final model
final_conf_matrix <- table(predict(model, newdata = test_data[, c(selected_features, "Attrition")], type = "raw"), test_data$Attrition)
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
