# Install and load necessary packages
# install.packages("caTools")
# install.packages("caret")

library(caTools)
library(caret)

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
data <- data[sample(nrow(data)), ]

# Split the data into training (70%) and testing (30%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Create a Naive Bayes model using 10-fold cross-validation
nb_model <- train(Attrition ~ ., data = train_data, method = "nb", trControl = train_control)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data)

# Confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, test_data$Attrition)
print(conf_matrix)

# Calculate accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", accuracy))

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix$byClass['Sensitivity']
print(paste("Sensitivity:", sensitivity))

# Calculate precision (positive predictive value)
precision <- conf_matrix$byClass['Pos Pred Value']
print(paste("Precision:", precision))

# Calculate specificity (true negative rate)
specificity <- conf_matrix$byClass['Neg Pred Value']
print(paste("Specificity:", specificity))