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

# Shuffle the rows of the data
set.seed(123) # Set seed for reproducibility
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.70)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Function for forward selection
forward_selection <- function(data, target, num_features) {
  selected_features <- c()
  available_features <- setdiff(names(data), target)
  
  for (i in 1:num_features) {
    importance_scores <- numeric(length(available_features))
    
    for (j in 1:length(available_features)) {
      features <- c(selected_features, available_features[j])
      formula <- as.formula(paste(target, "~", paste(features, collapse = "+")))
      model <- randomForest(formula, data = data, ntree = 600, mtry = sqrt(ncol(data)), nodesize = 10, importance = TRUE)
      importance_scores[j] <- importance(model)[, 2]  # Extracting importance for the positive class
    }
    
    best_feature <- available_features[which.max(importance_scores)]
    selected_features <- c(selected_features, best_feature)
    available_features <- setdiff(available_features, best_feature)
  }
  
  return(selected_features)
}

# Perform forward selection
selected_features <- forward_selection(train_data, "Attrition", 5)  # Select top 5 features

# Create a random forest model with selected features
formula <- as.formula(paste("Attrition ~", paste(selected_features, collapse = "+")))
model_rf <- randomForest(
  formula = formula,
  data = train_data,
  ntree = 600,          # Number of trees in the forest
  mtry = sqrt(length(selected_features)),  # Number of variables randomly sampled as candidates at each split
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
sensitivity_rf <- conf_matrix_rf[2, 2] / sum(test_data$Attrition == "Yes")
print(paste("Sensitivity:", sensitivity_rf))

# Calculate precision (positive predictive value)
precision_rf <- conf_matrix_rf[2, 2] / sum(predictions_rf == "Yes")
print(paste("Precision:", precision_rf))

# Calculate specificity (true negative rate)
specificity_rf <- conf_matrix_rf[1, 1] / sum(test_data$Attrition == "No")
print(paste("Specificity:", specificity_rf))
