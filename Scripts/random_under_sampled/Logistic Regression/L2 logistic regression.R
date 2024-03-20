# Install and load necessary packages
# install.packages("caTools")
# install.packages("glmnet")
library(caTools)
library(glmnet)

# Load the dataset
data <- read.csv("random_under_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Feature selection using Ridge (L2 regularization)
x <- as.matrix(train_data[, !names(train_data) %in% "Attrition"])
y <- as.factor(train_data$Attrition)

# Fit Ridge model
ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0)

# Extract selected features based on Ridge regularization
ridge_selected_features <- coef(ridge_model, s = "lambda.min")[-1, ]
ridge_selected_feature_indices <- which(ridge_selected_features != 0)

# Filter train and test data with Ridge selected features
train_data_ridge_selected <- train_data[, c(ridge_selected_feature_indices, which(names(train_data) == "Attrition"))]
test_data_ridge_selected <- test_data[, c(ridge_selected_feature_indices, which(names(test_data) == "Attrition"))]

# Train logistic regression model with Ridge selected features
model_ridge <- glm(Attrition ~ ., data = train_data_ridge_selected, family = "binomial")

# Make predictions on the test set with Ridge selected features
predictions_ridge <- predict(model_ridge, newdata = test_data_ridge_selected, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels_ridge <- ifelse(predictions_ridge > 0.5, 1, 0)

# Confusion matrix to evaluate the model with Ridge selected features
conf_matrix_ridge <- table(predicted_labels_ridge, test_data_ridge_selected$Attrition)
print(conf_matrix_ridge)

# Calculate evaluation parameters
# Calculate accuracy with Ridge selected features
accuracy_ridge <- sum(diag(conf_matrix_ridge)) / sum(conf_matrix_ridge)
print(paste("Accuracy with selected features (Ridge):", accuracy_ridge))

# Calculate sensitivity (true positive rate) with Ridge selected features
sensitivity_ridge <- conf_matrix_ridge[2, 2] / sum(conf_matrix_ridge[2, ])
print(paste("Sensitivity with selected features (Ridge):", sensitivity_ridge))

# Calculate precision (positive predictive value) with Ridge selected features
precision_ridge <- conf_matrix_ridge[2, 2] / sum(conf_matrix_ridge[, 2])
print(paste("Precision with selected features (Ridge):", precision_ridge))

# Calculate specificity (true negative rate) with Ridge selected features
specificity_ridge <- conf_matrix_ridge[1, 1] / sum(conf_matrix_ridge[1, ])
print(paste("Specificity with selected features (Ridge):", specificity_ridge))
