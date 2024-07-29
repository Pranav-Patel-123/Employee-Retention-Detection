# Install and load necessary packages
# install.packages("randomForest")
library(randomForest)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.70)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a random forest model
#model_rf <- randomForest(Attrition ~ ., data = train_data, ntree = 100, importance = TRUE)
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = train_data,
  ntree = 600,          # Number of trees in the forest
  mtry = sqrt(ncol(train_data)),  # Number of variables randomly sampled as candidates at each split
  nodesize = 10,         # Minimum size of terminal nodes
  importance = TRUE     # Compute and store variable importance
)


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
