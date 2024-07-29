# Install and load necessary packages
#install.packages("randomForest")
library(randomForest)

# Assuming 'Dataset.csv' is your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert "Attrition" column to factor
data$Attrition <- as.factor(data$Attrition)

# Identify categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert categorical columns to factors
data[, categorical_columns] <- lapply(data[, categorical_columns], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Train a random forest model
model_rf <- randomForest(Attrition ~ ., data = data, ntree = 100)

# Get feature importance
importance <- importance(model_rf)

# Set a threshold for low importance
threshold_importance <- 0.01

# Identify features with low importance
low_importance_features <- rownames(importance[importance[, "MeanDecreaseGini"] < threshold_importance, ])

# Combine all identified unnecessary columns
unnecessary_columns <- union(categorical_columns, low_importance_features)

# Print or inspect the unnecessary columns
print(unnecessary_columns)
