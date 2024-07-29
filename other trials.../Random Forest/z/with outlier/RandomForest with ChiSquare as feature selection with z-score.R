library(caTools)
library(randomForest)
library(caret)

# Load the dataset
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

# Function to calculate chi-square statistic
chi_square <- function(feature, target) {
  observed <- table(feature, target)
  expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
  chi_square <- sum((observed - expected)^2 / expected)
  return(chi_square)
}

# Calculate chi-square statistic for each feature
chi_square_scores <- sapply(train_data[, -which(names(train_data) == "Attrition")], 
                            function(x) chi_square(x, train_data$Attrition))

# Sort features based on chi-square scores
sorted_features <- names(sort(chi_square_scores, decreasing = TRUE))

# Select top features (you can change the number of features)
selected_features <- sorted_features[1:5]

# Print the finally selected features for training
print("Selected Features:")
print(selected_features)

# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Create a random forest model using 10-fold cross-validation
model <- train(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "rf", trControl = train_control)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data[, c(selected_features)], type = "raw")

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
