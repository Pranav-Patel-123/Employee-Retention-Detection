# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")
# install.packages("caret")

library(caTools)
library(rpart)
library(caret)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Manual scaling normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

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

# Define the control parameters for RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Perform Recursive Feature Elimination using decision tree
rfe_result <- rfe(x = train_data[, -which(names(train_data) == "Attrition")], 
                  y = train_data$Attrition,
                  sizes = c(1:(ncol(train_data) - 1)),
                  rfeControl = control)

# Get the selected features
selected_features <- predictors(rfe_result)

# Print finally selected features for training
cat("Finally selected features:", selected_features, "\n")

# Filter train and test data with selected features
train_data_rfe <- train_data[, c(selected_features, "Attrition")]
test_data_rfe <- test_data[, c(selected_features, "Attrition")]

# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Create a decision tree model using 10-fold cross-validation
model <- train(Attrition ~ ., data = train_data_rfe, method = "rpart", trControl = train_control)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data_rfe, type = "raw")

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data_rfe$Attrition)
print(conf_matrix)

# Calculate evaluation parameters
# Calculate accuracy with RFE selected features
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy with selected features (RFE):", accuracy))

# Calculate sensitivity (true positive rate) with RFE selected features
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Sensitivity with selected features (RFE):", sensitivity))

# Calculate precision (positive predictive value) with RFE selected features
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste("Precision with selected features (RFE):", precision))

# Calculate specificity (true negative rate) with RFE selected features
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
print(paste("Specificity with selected features (RFE):", specificity))
