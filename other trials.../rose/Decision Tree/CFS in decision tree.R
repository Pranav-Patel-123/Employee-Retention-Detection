# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")

library(caTools)
library(rpart)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

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

# Convert data to numeric for correlation calculation
numeric_data <- train_data[, sapply(train_data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Perform feature selection based on correlation
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.7)
selected_features <- names(numeric_data)[-1][!names(numeric_data)[-1] %in% correlated_features]

# Create a decision tree model using the selected features
model <- rpart(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "class")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data[, c(selected_features, "Attrition")], type = "class")

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data$Attrition)
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