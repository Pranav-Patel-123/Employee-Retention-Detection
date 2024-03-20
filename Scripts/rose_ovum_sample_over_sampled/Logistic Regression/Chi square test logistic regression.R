# Install and load necessary packages
# install.packages("caTools")
library(caTools)

# Load the dataset
data <- read.csv("rose_ovum_sample_over_sampled.csv")

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

# Function to perform Chi-square test and select significant features
perform_chi_sq <- function(data) {
  p_values <- sapply(data[, -which(names(data) == "Attrition")], function(feature) {
    suppressWarnings(chi_sq_test <- chisq.test(table(feature, data$Attrition)))
    chi_sq_test$p.value
  })
  selected_features <- names(p_values)[p_values < 0.05]
  return(selected_features)
}

# Perform Chi-square test for feature selection
selected_features <- perform_chi_sq(train_data)

# Train logistic regression model with selected features
model_chi_sq <- glm(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], family = "binomial")

# Make predictions on the test set with selected features
predictions_chi_sq <- predict(model_chi_sq, newdata = test_data[, c(selected_features, "Attrition")], type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels_chi_sq <- ifelse(predictions_chi_sq > 0.5, 1, 0)

# Confusion matrix to evaluate the model with selected features
conf_matrix_chi_sq <- table(predicted_labels_chi_sq, test_data$Attrition)
print(conf_matrix_chi_sq)

# Calculate evaluation parameters
# Calculate accuracy with selected features
accuracy_chi_sq <- sum(diag(conf_matrix_chi_sq)) / sum(conf_matrix_chi_sq)
print(paste("Accuracy with selected features (Chi-square):", accuracy_chi_sq))

# Calculate sensitivity (true positive rate) with selected features
sensitivity_chi_sq <- conf_matrix_chi_sq[2, 2] / sum(conf_matrix_chi_sq[2, ])
print(paste("Sensitivity with selected features (Chi-square):", sensitivity_chi_sq))

# Calculate precision (positive predictive value) with selected features
precision_chi_sq <- conf_matrix_chi_sq[2, 2] / sum(conf_matrix_chi_sq[, 2])
print(paste("Precision with selected features (Chi-square):", precision_chi_sq))

# Calculate specificity (true negative rate) with selected features
specificity_chi_sq <- conf_matrix_chi_sq[1, 1] / sum(conf_matrix_chi_sq[1, ])
print(paste("Specificity with selected features (Chi-square):", specificity_chi_sq))
