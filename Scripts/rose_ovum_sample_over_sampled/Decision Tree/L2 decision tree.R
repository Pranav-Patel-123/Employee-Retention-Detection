# Install and load necessary packages
# install.packages("caTools")
# install.packages("glmnet")

library(caTools)
library(glmnet)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Define predictors and response variable
x <- as.matrix(train_data[, !names(train_data) %in% "Attrition"])
y <- as.vector(train_data$Attrition)

# Fit Ridge model
ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0)

# Extract selected features based on Ridge regularization
ridge_selected_features <- coef(ridge_model, s = "lambda.min")[-1, ]
ridge_selected_feature_indices <- which(ridge_selected_features != 0)

# Filter train and test data with Ridge selected features
train_data_ridge_selected <- train_data[, c(ridge_selected_feature_indices, which(names(train_data) == "Attrition"))]
test_data_ridge_selected <- test_data[, c(ridge_selected_feature_indices, which(names(test_data) == "Attrition"))]

# Create a decision tree model using the Ridge selected features
model <- rpart(Attrition ~ ., data = train_data_ridge_selected, method = "class")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data_ridge_selected, type = "class")

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data_ridge_selected$Attrition)
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