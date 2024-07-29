# Load the required libraries
library(caTools)
library(glmnet)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Perform LASSO feature selection
x <- model.matrix(Attrition ~ ., data)[,-1]
y <- as.numeric(data$Attrition) - 1
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
selected_features <- rownames(coef(lasso_model, s = "lambda.min"))[-1]

# Print selected features
if (length(selected_features) >= 10) {
  cat("Top 10 selected features based on LASSO (L1 Regularization):\n", selected_features[1:10], "\n")
} else {
  cat("Less than 10 features selected.\n")
}