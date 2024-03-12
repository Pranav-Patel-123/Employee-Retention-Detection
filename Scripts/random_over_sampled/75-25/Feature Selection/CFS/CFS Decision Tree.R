# Install and load necessary packages
# install.packages("rpart")

library(rpart)
library(caret)
library(caTools)


# Load the dataset
data <- read.csv("random_over_sampled.csv")  # Replace with your actual file name

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Convert data to numeric for correlation calculation
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Perform feature selection based on correlation
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.4)
selected_features <- names(numeric_data)[-1][!names(numeric_data)[-1] %in% correlated_features]

# Select only the relevant columns
data <- data[, c("Attrition", selected_features)]

# Set the seed for reproducibility
set.seed(123)

# Specify the number of samples for the training set
num_train_samples <- round(0.75 * nrow(data))

# Create the training set
train_data <- data[1:num_train_samples, ]

# Create the test set
test_data <- data[(num_train_samples + 1):nrow(data), ]
tree_model <- train(Attrition ~ ., data = train_data, method = "rpart", trControl = ctrl)
#Build a decision tree model
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class")
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", minsplit = 10, minbucket = 5)
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", cp = 0.01)
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", maxdepth = 5)
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", maxcompete = 0, maxsurrogate = 0)
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", cp = 0, xval = 10)
#tree_model <- rpart(Attrition ~ ., data = train_data, method = "class", cp = 0.01, minsplit = 10, minbucket = 5, maxdepth = 5)

# Make predictions on the test set
predictions <- predict(tree_model, test_data, type = "prob")[, 2]  # Use probability of class 1

# Adjust the decision threshold
threshold <- 0.22  # Experiment with different threshold values
binary_predictions <- ifelse(predictions > threshold, 1, 0)

# Confusion matrix to evaluate the model
conf_matrix <- table(binary_predictions, test_data$Attrition)
print(conf_matrix)

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
