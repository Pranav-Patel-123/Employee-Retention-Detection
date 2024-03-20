# Install and load necessary packages
# install.packages(c("randomForest", "caTools", "caret"))

library(randomForest)
library(caret)
library(caTools)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

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

# Convert data to numeric for correlation calculation
numeric_data <- train_data[, sapply(train_data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Perform feature selection based on correlation
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.8)
selected_features <- names(numeric_data)[-1][!names(numeric_data)[-1] %in% correlated_features]

# Select only the relevant columns
train_data <- train_data[, c("Attrition", selected_features)]
test_data <- test_data[, c("Attrition", selected_features)]

# Create a train control for k-fold cross-validation within the 75% training set
train_control <- trainControl(
  method = "cv",                # Use k-fold cross-validation
  number = num_folds,           # Number of folds
  savePredictions = "final",    # Save final predictions on the test set
  classProbs = TRUE,            # Enable class probabilities
  index = createMultiFolds(train_data$Attrition, k = num_folds, times = 1)  # k-fold within the 75% training set
)

# Create a random forest model using train function
model_rf <- randomForest(
  formula = Attrition ~ .,
  data = train_data,
  ntree = 800,          # Number of trees in the forest
  mtry = sqrt(ncol(train_data)),  # Number of variables randomly sampled as candidates at each split
  nodesize = 5,         # Minimum size of terminal nodes
  importance = TRUE     # Compute and store variable importance
)

# Print the model information
print(model_rf)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test_data[, -which(names(test_data) == "Attrition")])

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
