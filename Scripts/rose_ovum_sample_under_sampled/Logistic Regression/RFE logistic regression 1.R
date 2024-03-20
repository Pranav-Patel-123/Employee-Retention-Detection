# Install and load necessary packages
# install.packages("caTools")
# install.packages("glmnet")
# install.packages("caret")
library(caTools)
library(glmnet)
library(caret)

# Load the dataset
data <- read.csv("rose_ovum_sample_under_sampled.csv")

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

# Define the control parameters for RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Perform Recursive Feature Elimination using Random Forest
rfe_result <- rfe(x = train_data[, -which(names(train_data) == "Attrition")], 
                  y = train_data$Attrition,
                  sizes = c(1:ncol(train_data) - 1),
                  rfeControl = control)

# Get the selected features
selected_features <- predictors(rfe_result)

# Filter train and test data with selected features
train_data_rfe <- train_data[, c(selected_features, "Attrition")]
test_data_rfe <- test_data[, c(selected_features, "Attrition")]

# Train logistic regression model with RFE selected features
model_logistic <- glm(Attrition ~ ., data = train_data_rfe, family = "binomial")

# Make predictions on the test set with RFE selected features
predictions <- predict(model_logistic, newdata = test_data_rfe, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix to evaluate the model with RFE selected features
conf_matrix <- table(predicted_labels, test_data_rfe$Attrition)
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
