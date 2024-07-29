# Install and load necessary packages
# install.packages("randomForest")
# install.packages("glmnet")
library(randomForest)
library(glmnet)

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

# Shuffle the rows of the data
set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Feature selection using Elastic Net regularization (combination of L1 and L2)
# Convert the data into a matrix format
x <- as.matrix(train_data[, !names(train_data) %in% "Attrition"])
y <- as.factor(train_data$Attrition)

# Fit Elastic Net model
elastic_net_model <- cv.glmnet(x, y, family = "binomial", alpha = 0.5)

# Extract selected features based on Elastic Net regularization
selected_features <- coef(elastic_net_model, s = "lambda.min")[-1, ]
selected_feature_indices <- which(selected_features != 0)

# Filter train and test data with selected features
train_data_selected <- train_data[, c(selected_feature_indices, which(names(train_data) == "Attrition"))]
test_data_selected <- test_data[, c(selected_feature_indices, which(names(test_data) == "Attrition"))]

# Create a random forest model with selected features
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = train_data_selected,
  ntree = 600,          # Number of trees in the forest
  mtry = sqrt(ncol(train_data_selected) - 1),  # Number of variables randomly sampled as candidates at each split (-1 for target variable)
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE     # Compute and store variable importance
)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test_data_selected)

# Confusion matrix to evaluate the model
conf_matrix_rf <- table(predictions_rf, test_data_selected$Attrition)
print(conf_matrix_rf)

# Calculate accuracy
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Accuracy:", accuracy_rf))

# Calculate sensitivity (true positive rate)
sensitivity_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ])
print(paste("Sensitivity:", sensitivity_rf))

# Calculate precision (positive predictive value)
precision_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2])
print(paste("Precision:", precision_rf))

# Calculate specificity (true negative rate)
specificity_rf <- conf_matrix_rf[1, 1] / sum(conf_matrix_rf[1, ])
print(paste("Specificity:", specificity_rf))
