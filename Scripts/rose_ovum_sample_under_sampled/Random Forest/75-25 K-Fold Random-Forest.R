# Install and load necessary packages
# install.packages(c("randomForest", "caTools"))
library(randomForest)
library(caret)
library(caTools)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_under_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Specify the number of folds (k=10)
num_folds <- 10

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a train control for k-fold cross-validation within the 75% training set
train_control <- trainControl(
  method = "cv",                # Use k-fold cross-validation
  number = num_folds,           # Number of folds
  savePredictions = "final",    # Save final predictions on the test set
  classProbs = TRUE,            # Enable class probabilities
  index = createMultiFolds(train_data$Attrition, k = num_folds, times = 1)  # k-fold within the 75% training set
)

# Create a random forest model using train function
model_rf <- train(
  x = train_data[, -which(names(train_data) == "Attrition")],
  y = train_data$Attrition,
  method = "rf",
  trControl = train_control,
  tuneGrid = expand.grid(mtry = sqrt(ncol(train_data))),  # Number of variables randomly sampled as candidates at each split
  nodesize = 10,               # Minimum size of terminal nodes
  importance = TRUE            # Compute and store variable importance
)

# Print the model information
print(model_rf)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test_data)

# Confusion matrix to evaluate the model
conf_matrix_rf <- table(predictions_rf, test_data$Attrition)
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
