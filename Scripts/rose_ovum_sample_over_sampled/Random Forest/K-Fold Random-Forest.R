# Install and load necessary packages
# install.packages("randomForest")
library(randomForest)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Create a random forest model with k-fold cross-validation
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = data,
  ntree = 750,          # Number of trees in the forest
  mtry = sqrt(ncol(data)),  # Number of variables randomly sampled as candidates at each split
  nodesize = 15,         # Minimum size of terminal nodes
  importance = TRUE,    # Compute and store variable importance
  nfold = 10            # Number of folds for cross-validation
)

# Print the confusion matrix
print(model_rf$confusion)

# Calculate accuracy
accuracy_rf <- sum(diag(model_rf$confusion)) / sum(model_rf$confusion)
print(paste("Accuracy:", accuracy_rf))

# Calculate sensitivity (true positive rate)
sensitivity_rf <- model_rf$confusion[2, 2] / sum(model_rf$confusion[2, ])
print(paste("Sensitivity:", sensitivity_rf))

# Calculate precision (positive predictive value)
precision_rf <- model_rf$confusion[2, 2] / sum(model_rf$confusion[, 2])
print(paste("Precision:", precision_rf))

# Calculate specificity (true negative rate)
specificity_rf <- model_rf$confusion[1, 1] / sum(model_rf$confusion[1, ])
print(paste("Specificity:", specificity_rf))
