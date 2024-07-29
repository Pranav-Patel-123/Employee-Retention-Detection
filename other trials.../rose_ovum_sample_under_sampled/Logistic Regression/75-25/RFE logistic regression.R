# Install and load necessary packages
# install.packages("caTools")
# install.packages("glmnet")
# install.packages("caret")
library(caTools)
library(glmnet)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
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

# Define control parameters for RFE
ctrl <- rfeControl(functions = rfFuncs,  # Use Random Forest for feature selection
                   method = "cv",        # Use cross-validation
                   number = 10)          # Number of folds for cross-validation

# Perform Recursive Feature Elimination (RFE)
rfe_result <- rfe(x = train_data[, -which(names(train_data) == "Attrition")],  # Features
                  y = train_data$Attrition,                                # Target variable
                  sizes = c(1:10),                                          # Number of features to consider at each iteration
                  rfeControl = ctrl)

# Get the selected features
selected_features <- predictors(rfe_result)

# Train logistic regression model with selected features
model_rfe <- glm(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], family = "binomial")

# Make predictions on the test set with selected features
predictions_rfe <- predict(model_rfe, newdata = test_data[, c(selected_features, "Attrition")], type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels_rfe <- ifelse(predictions_rfe > 0.5, 1, 0)

# Confusion matrix to evaluate the model with selected features
conf_matrix_rfe <- table(predicted_labels_rfe, test_data$Attrition)
print(conf_matrix_rfe)

# Calculate evaluation parameters
# Calculate accuracy with selected features
accuracy_rfe <- sum(diag(conf_matrix_rfe)) / sum(conf_matrix_rfe)
print(paste("Accuracy with selected features (RFE):", accuracy_rfe))

# Calculate sensitivity (true positive rate) with selected features
sensitivity_rfe <- conf_matrix_rfe[2, 2] / sum(conf_matrix_rfe[2, ])
print(paste("Sensitivity with selected features (RFE):", sensitivity_rfe))

# Calculate precision (positive predictive value) with selected features
precision_rfe <- conf_matrix_rfe[2, 2] / sum(conf_matrix_rfe[, 2])
print(paste("Precision with selected features (RFE):", precision_rfe))

# Calculate specificity (true negative rate) with selected features
specificity_rfe <- conf_matrix_rfe[1, 1] / sum(conf_matrix_rfe[1, ])
print(paste("Specificity with selected features (RFE):", specificity_rfe))
